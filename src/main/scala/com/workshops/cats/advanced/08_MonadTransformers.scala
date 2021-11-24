package com.workshops.cats.advanced

import cats.data.{EitherT, OptionT}
import cats.effect.IO
import cats.syntax.applicative._
import cats.~>

object MonadTransformersIntro {

  /** The final topic for today is monad transformers.
    * So far we've seen a nice way to chain our executions via .map and .flatMap
    * But what if we need to stack different effects together?
    * For example Option with Either?
    * The naive way would be to simply define our return type as follows:
    */
  trait User
  trait GiftCard
  trait Gift

  def fetchUser(): Either[String, Option[User]] = ???

  def fetchGiftCard(user: User): Either[String, Option[GiftCard]] = ???

  def pickGift(giftCard: GiftCard): Either[String, Option[Gift]] = ???

  def gift: Either[String, Option[Gift]] = fetchUser().flatMap {
    case Some(user) =>
      fetchGiftCard(user).flatMap {
        case Some(gc) => pickGift(gc)
        case None     => Right(None)
      }
    case None => Right(None)
  }

  /** Quite cumbersome, isn't it?
    * What if we had a better way?
    * It turns out there is one: monad transformers.
    * For example EitherT from Cats composes Either with other monads, OptionT composes Option, etc.
    * The type signatures of monad transformers are written from the inside out, thus an EitherT[Option, String, A] is
    * a wrapper for an Option[Either[String, A]].
    * It is often useful to use type aliases when writing transformer types for deeply nested monads.
    */
  import cats.instances.either._ // .map(_.author) requires a Functor[Option] instance in implicit scope

  type AOrErr[A] = Either[Error, A] // the trick as OptionT requires a type constructor with one argument
  type Effect[A] = OptionT[AOrErr, A]

  def fetchUserT(): Effect[User] = ???

  def fetchGiftCardT(user: User): Effect[GiftCard] = ???

  def pickGiftT(giftCard: GiftCard): Effect[Gift] = ???

  def giftT: OptionT[AOrErr, Gift] = for {
    user <- fetchUserT()
    gc   <- fetchGiftCardT(user)
    gift <- pickGiftT(gc)
  } yield gift

  /** We now can deal with all the complexity on the very top of our program.
    */
  def maybeGift: Option[Gift] = giftT.value match {
    case Left(_)      => None
    case Right(value) => value
  }

  /** Drawbacks.
    * The cats implementation of monad transformers comes with a burden of extra memory allocation for each Wrapper.
    * Thus, the deeper our stack is the more memory we need per each call of .map or .flatMap in the context of the wrapper.
    * Good news are that it's quite rare situation (in my personal experience) when you need more than one transformer stacked.
    *
    * Some realistic example might be roughly described as follows: (except that in real apps we don't use Future when possible).
    */
  // inner layer of our app
  trait DBError
  type DbEff[A] = EitherT[IO, DBError, A]

  // business logic
  trait UserServiceError
  final case class UnderlyingError(nested: DBError) extends UserServiceError
  type UserServiceEff[A] = EitherT[IO, UserServiceError, A]

  // rest api layer
  final case class ServiceFailure(nested: UserServiceError) extends Exception(nested.toString)
  type HttpEff[A] = IO[A]

  /** What we want in most cases we'd like to have a transformation from DbEff context into UserServiceEff and from UserServiceEff into HttpEff
    * That models a natural flow: We try to fetch something from database from service layer and return it through http api.
    * One of the ways to move from one effect context to another is `natural transformation`, or FunctionK.
    * Such function transforms values from one type that takes single type parameter to another, i.e. F[_] ~> G[_]
    * The trick is that we don't really care what's inside if such a transformation exists, we can apply it to any F[_].
    */

  val dbToService: DbEff ~> UserServiceEff = new (DbEff ~> UserServiceEff) {
    override def apply[A](fa: DbEff[A]): UserServiceEff[A] = fa.leftMap(UnderlyingError.apply)
  }

  val serviceToHttp: UserServiceEff ~> HttpEff = new (UserServiceEff ~> HttpEff) {
    override def apply[A](fa: UserServiceEff[A]): HttpEff[A] = fa.leftMap(ServiceFailure.apply).rethrowT
  }

  val dbMethod: DbEff[String] = "hello".pure[DbEff]

  val serviceMethod: UserServiceEff[String] = "world".pure[UserServiceEff]

  val httpMethod: HttpEff[String] = {
    val resultF: UserServiceEff[String] =
      for {
        fromDb      <- dbToService(dbMethod)
        fromService <- serviceMethod
      } yield s"$fromDb $fromService"

    serviceToHttp(resultF)
  }

}

// More closer to the real code

import cats.Monad
import cats.data.EitherT
import cats.effect.IO
import cats.implicits._

object For {

  // API
  type Error
  type UserId
  type OrderId
  type Item

  trait Repository {

    def getFriends(userId: UserId):  Either[Error, List[UserId]] = ???
    def getOrder(userId:   UserId):  Either[Error, OrderId]      = ???
    def getItems(orderId:  OrderId): Either[Error, List[Item]]   = ???
  }

  class Task1(repo: Repository) {
    // find all friends for users passed
    def friends(users: List[UserId]): Either[Error, List[UserId]] = {
      users
        .map(x => repo.getFriends(x))
        .sequence
        .map(x => x.flatten)
    }
  }

  class Service(repo: Repository) {

    def friendsOrders(userId: UserId): Either[Error, List[Item]] = {
      for {
        friendsIds <- repo.getFriends(userId)
        orderIds   <- friendsIds.traverse(id => repo.getOrder(id))
        itemsIds   <- orderIds.traverse(id => repo.getItems(id))
      } yield itemsIds.flatten
    }
  }

}

object MonadTransformers {

  // API
  type Error
  type UserId
  type OrderId
  type Item

  trait Repository {

    def getFriends(userId: UserId):  IO[Either[Error, List[UserId]]] = ???
    def getOrder(userId:   UserId):  IO[Either[Error, OrderId]]      = ???
    def getItems(orderId:  OrderId): IO[Either[Error, List[Item]]]   = ???
  }

  // implement
  class Service(repo: Repository) {

    def friendsOrders(userId: UserId): IO[Either[Error, List[Item]]] = {
      val res = for {
        friendsIds <- EitherT(repo.getFriends(userId))
        orderIds   <- friendsIds.traverse(id => EitherT(repo.getOrder(id)))
        itemsIds   <- orderIds.traverse(id => EitherT(repo.getItems(id)))
      } yield itemsIds.flatten
      res.value
    }
  }
}

object Generic {

  // API
  type Error
  type UserId
  type OrderId
  type Item

  trait Repository[F[_]] {

    def getFriends(userId: UserId): F[List[UserId]] = ???

    def getOrder(userId: UserId): F[OrderId] = ???

    def getItems(orderId: OrderId): F[List[Item]] = ???
  }

  // implement
  class Service[F[_]: Monad](repo: Repository[F]) {

    def friendsOrders(userId: UserId): F[List[Item]] = ???
  }

  //  new Repository of IO
  //  new Repository of Either
  //  new Repository of EitherT
  //  new Repository of IO Either

}
