package fp

import eu.timepit.refined.api.Refined
import eu.timepit.refined.string.Url
import argonaut._, Argonaut._
import simulacrum._
import java.net.URLEncoder
import scalaz.IList
import scalaz._, Scalaz._
import scala.concurrent.duration._

object OAth2 {
  import UrlEncodedWriter.ops._
  import UrlQueryWriter.ops._

  final case class AuthRequest(
    redirect_uri: String Refined Url,
    scope: String,
    client_id: String,
    prompt: String = "consent",
    response_type: String = "code",
    access_type: String = "offline"
  )

  object AuthRequest {
    implicit val query: UrlQueryWriter[AuthRequest] = { a =>
      UrlQuery(
        List(
          "redirect_uri" -> a.redirect_uri.value,
          "scope" -> a.scope,
          "client_id" -> a.client_id,
          "prompt" -> a.prompt,
          "response_type" -> a.response_type,
          "access_type" -> a.access_type
        )
      )
    }
  }

  final case class AccessRequest(
    code: String,
    redirect_uri: String Refined Url,
    client_id: String,
    client_secret: String,
    scope: String = "",
    grant_type: String = "authorization_code"
  )

  object AccessRequest {
    implicit val encoded: UrlEncodedWriter[AccessRequest] = { a =>
      IList(
        "code" -> a.code.toUrlEncoded,
        "redirect_uri" -> a.redirect_uri.toUrlEncoded,
        "client_id" -> a.client_id.toUrlEncoded,
        "client_secret" -> a.client_secret.toUrlEncoded,
        "scope" -> a.scope.toUrlEncoded,
        "grant_type" -> a.grant_type.toUrlEncoded
      ).toUrlEncoded
    }
  }

  final case class AccessResponse(
    access_token: String,
    token_type: String,
    expires_in: Long,
    refresh_token: String
  )

  object AccessResponse {
    implicit def AccessResponseCodec: CodecJson[AccessResponse] =
      casecodec4(AccessResponse.apply, AccessResponse.unapply)(
        "access_token",
        "token_type",
        "expires_in",
        "refresh_token"
      )
  }

  final case class RefreshRequest(
    client_secret: String,
    refresh_token: String,
    client_id: String,
    grant_type: String = "refresh_token"
  )

  object RefreshRequest {
    implicit val encoded: UrlEncodedWriter[RefreshRequest] = { r =>
      IList(
        "client_secret" -> r.client_secret.toUrlEncoded,
        "refresh_token" -> r.refresh_token.toUrlEncoded,
        "client_id" -> r.client_id.toUrlEncoded,
        "grant_type" -> r.grant_type.toUrlEncoded
      ).toUrlEncoded
    }
  }

  final case class RefreshResponse(
    access_token: String,
    token_type: String,
    expires_in: Long
  )

  object RefreshResponse {
    implicit def RefreshResponseCodec: CodecJson[RefreshResponse] =
      casecodec3(RefreshResponse.apply, RefreshResponse.unapply)(
        "access_token",
        "token_type",
        "expires_in"
      )
  }

  final case class UrlQuery(params: List[(String, String)])

  @typeclass trait UrlQueryWriter[A] {
    def toUrlQuery(a: A): UrlQuery
  }

  @typeclass trait UrlEncodedWriter[A] {
    def toUrlEncoded(a: A): String Refined UrlEncoded
  }

  object UrlEncodedWriter {
    implicit val encoded: UrlEncodedWriter[String Refined UrlEncoded] = identity

    implicit val string: UrlEncodedWriter[String] =
      (s => Refined.unsafeApply(URLEncoder.encode(s, "UTF-8")))

    implicit val url: UrlEncodedWriter[String Refined Url] = (
      s => s.value.toUrlEncoded
    )

    implicit val long: UrlEncodedWriter[Long] =
      (s => Refined.unsafeApply(s.toString))

    implicit def ilist[K: UrlEncodedWriter, V: UrlEncodedWriter]
      : UrlEncodedWriter[IList[(K, V)]] = { m =>
      val raw = m
        .map {
          case (k, v) => k.toUrlEncoded.value + "=" + v.toUrlEncoded.value
        }
        .intercalate("&")
      Refined.unsafeApply(raw)
    }
  }

  trait JsonClient[F[_]] {
    def get[A: CodecJson](
      uri: String Refined Url,
      headers: IList[(String, String)]
    ): F[A]

    def post[P: UrlEncodedWriter, A: CodecJson](
      uri: String Refined Url,
      payload: P,
      headers: IList[(String, String)] = IList.empty
    ): F[A]
  }

  final case class CodeToken(token: String, redirect_uri: String Refined Url)

  trait UserInteraction[F[_]] {
    def start: F[String Refined Url]
    def open(uri: String Refined Url): F[Unit]
    def stop: F[CodeToken]
  }

  trait LocalClock[F[_]] {
    def now: F[Epoch]
  }

  final case class ServerConfig(
    auth: String Refined Url,
    access: String Refined Url,
    refresh: String Refined Url,
    scope: String,
    clientId: String,
    clientSecret: String
  )
  final case class RefreshToken(token: String)
  final case class BearerToken(token: String, expires: Epoch)

  class OAuth2Client[F[_]: Monad](config: ServerConfig)(
    user: UserInteraction[F],
    client: JsonClient[F],
    clock: LocalClock[F]
  ) {
    def authenticate: F[CodeToken] =
      for {
        callback <- user.start
        params = AuthRequest(callback, config.scope, config.clientId)
        _ <- user.open(config.auth)
        code <- user.stop
      } yield code

    def access(code: CodeToken): F[(RefreshToken, BearerToken)] =
      for {
        request <- AccessRequest(
          code.token,
          code.redirect_uri,
          config.clientId,
          config.clientSecret
        ).pure[F]
        msg <- client
          .post[AccessRequest, AccessResponse](config.access, request)
        time <- clock.now
        expires = time + msg.expires_in.seconds
        refresh = RefreshToken(msg.refresh_token)
        bearer = BearerToken(msg.access_token, expires)
      } yield (refresh, bearer)

    def bearer(refresh: RefreshToken): F[BearerToken] =
      for {
        request <- RefreshRequest(
          config.clientSecret,
          refresh.token,
          config.clientId
        ).pure[F]
        msg <- client
          .post[RefreshRequest, RefreshResponse](config.refresh, request)
        time <- clock.now
        expires = time + msg.expires_in.seconds
        bearer = BearerToken(msg.access_token, expires)
      } yield bearer
  }
}
