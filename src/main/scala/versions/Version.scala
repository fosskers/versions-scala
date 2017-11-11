package versions

import scala.annotation.tailrec

import cats._
import cats.implicits._
import monocle.macros.Lenses

// --- //

// sealed trait Version

/** An (Ideal) version number that conforms to Semantic Versioning.
  * This is a ''prescriptive'' parser, meaning it follows the SemVer standard.
  *
  * Legal semvers are of the form: MAJOR.MINOR.PATCH-PREREL+META
  *
  * Example: 1.2.3-r1+commithash
  *
  * Extra Rules:
  *   - Pre-release versions have ''lower'' precedence than normal versions.
  *   - Build metadata does not affect version precedence.
  *
  * For more information, see [[http://semver.org]].
  */
@Lenses case class SemVer(major: Int, minor: Int, patch: Int, prerel: List[VChunk], meta: List[VChunk])

object SemVer {

  implicit val semverShow: Show[SemVer] = derive.show[SemVer]

  /** Two [[SemVer]] are only equal if all fields except metadata are equal. */
  implicit val semverEq: Eq[SemVer] = new Eq[SemVer] {
    def eqv(x: SemVer, y: SemVer): Boolean =
      x.major === y.major && x.minor === y.minor && x.patch === y.patch && x.prerel === y.prerel
  }

  /** Build metadata does not affect version precedence. */
  implicit val semverOrd: Order[SemVer] = new Order[SemVer] {
    def compare(x: SemVer, y: SemVer): Int =
      Order.compare((x.major, x.minor, x.patch), (y.major, y.minor, y.patch)) match {
        case 0 => (x.prerel, y.prerel) match {
          case (Nil, Nil) => 0
          case (Nil, _)   => 1
          case (_, Nil)   => -1
          case _          => Order[List[VChunk]].compare(x.prerel, y.prerel)
        }
        case n => n
      }
  }

  implicit val semverSemigroup: Semigroup[SemVer] = new Semigroup[SemVer] {
    def combine(x: SemVer, y: SemVer): SemVer =
      SemVer(x.major + y.major, x.minor + y.minor, x.patch + y.patch, x.prerel ++ y.prerel, x.meta ++ y.meta)
  }

  implicit val semverMonoid: Monoid[SemVer] = new Monoid[SemVer] {
    def combine(x: SemVer, y: SemVer): SemVer = Semigroup.combine(x, y)

    def empty: SemVer = SemVer(0, 0, 0, Nil, Nil)
  }

}

@Lenses case class Version(epoch: Option[Int], chunks: List[VChunk], release: List[VChunk]) {
  /** Set a Version's epoch to `None`. */
  private[versions] def wipe: Version = Version(None, chunks, release)
}

object Version {

  implicit val versionEq: Eq[Version] = derive.eq[Version]
  implicit val versionShow: Show[Version] = derive.show[Version]

  implicit val versionOrd: Order[Version] = new Order[Version] {

    /** Compare two `VChunk`s. */
    @tailrec private[this] def chunks(ls: List[VUnit], rs: List[VUnit]): Int = (ls, rs) match {
      case (Nil, Nil) => 0

      /* If we've recursed this far and one side has
       * fewer chunks, it must be the "greater" version. A Chunk break only occurs in
       * a switch from digits to letters and vice versa, so anything "extra" must be
       * an `rc` marking or similar. Consider `1.1` compared to `1.1rc1`.
       */
      case (Nil, _)   => 1
      case (_, Nil)   => -1

      /* The usual case. */
      case (VUnit.Digits(n) :: ns, VUnit.Digits(m) :: ms) if n > m => 1
      case (VUnit.Digits(n) :: ns, VUnit.Digits(m) :: ms) if n < m => -1
      case (VUnit.Digits(n) :: ns, VUnit.Digits(m) :: ms) => chunks(ns, ms)
      case (VUnit.Text(n)   :: ns, VUnit.Text(m)   :: ms) if n > m => 1
      case (VUnit.Text(n)   :: ns, VUnit.Text(m)   :: ms) if n < m => -1
      case (VUnit.Text(n)   :: ns, VUnit.Text(m)   :: ms) => chunks(ns, ms)

      /* An arbitrary decision to prioritize digits over letters. */
      case (VUnit.Digits(_) :: _, VUnit.Text(_) :: _) => 1
      case (VUnit.Text(_) :: _, VUnit.Digits(_) :: _) => -1
    }

    def compare(x: Version, y: Version): Int = (x, y) match {
      /* The obvious base case. */
      case (Version(_, Nil, Nil), Version(_, Nil, Nil)) => 0

      /* For the purposes of Versions with epochs, `None` is the same as `Some(0)`,
       * so we need to compare their actual version numbers.
       */
      case (v0@(Version(Some(0), _, _)), v1@(Version(None, _, _))) => compare(v0.wipe, v1)
      case (v0@(Version(None, _, _)), v1@(Version(Some(0), _, _))) => compare(v0, v1.wipe)

      /* If a version has an epoch > 1 and the other has no epoch, the first will
       * be considered greater.
       */
      case (Version(Some(_), _, _), Version(None, _, _)) => 1
      case (Version(None, _, _), Version(Some(_), _, _)) => 0

      /* If two epochs are equal, we need to compare their actual version numbers.
       * Otherwise, the comparison of the epochs is the only thing that matters.
       */
      case (v0@(Version(Some(n), _, _)), v1@(Version(Some(m), _, _))) if n === m => compare(v0.wipe, v1.wipe)
      case (v0@(Version(Some(n), _, _)), v1@(Version(Some(m), _, _))) => Order[Int].compare(n, m)

      /* If the two Versions were otherwise equal and recursed down this far,
       * we need to compare them by their "release" values.
       */
      case (Version(_, Nil, rs), Version(_, Nil, ss)) => compare(Version(None, rs, Nil), Version(None, ss, Nil))

      /* If one side has run out of chunks to compare but the other hasn't,
       * the other must be newer.
       */
      case (Version(_, _, _), Version(_, Nil, _)) => 1
      case (Version(_, Nil, _), Version(_, _, _)) => -1

      /* The usual case. If first VChunks of each Version is equal, then we
       * keep recursing. Otherwise, we don't need to check further. Consider `1.2`
       * compared to `1.1.3.4.5.6`.
       */
      case (Version(_, (a :: as), rs), Version(_, (b :: bs), ss)) => chunks(a, b) match {
        case 0 => compare(Version(None, as, rs), Version(None, bs, ss))
        case n => n
      }
    }
  }
}

/** A single unit of a version. May be digits or a string of characters.
  * Groups of these are called [[VChunk]] s, and are the identifiers separated
  * by periods in the source.
  */
sealed trait VUnit

object VUnit {

  case class Digits(digits: Int) extends VUnit
  case class Text(text: String) extends VUnit

  implicit val vunitShow: Show[VUnit] = derive.show[VUnit]

  implicit val vunitEq: Eq[VUnit] = new Eq[VUnit] {
    def eqv(x: VUnit, y: VUnit): Boolean = (x, y) match {
      case (Digits(_), Text(_))   => false
      case (Text(_),   Digits(_)) => false
      case (Digits(d), Digits(e)) => d === e
      case (Text(t),   Text(s))   => t === s
    }
  }

  implicit val vunitOrd: Order[VUnit] = new Order[VUnit] {
    def compare(x: VUnit, y: VUnit): Int = (x, y) match {
      case (Digits(_), Text(_))   => 1
      case (Text(_),   Digits(_)) => -1
      case (Digits(d), Digits(e)) => Order[Int].compare(d, e)
      case (Text(t),   Text(s))   => Order[String].compare(t, s)
    }
  }
}
