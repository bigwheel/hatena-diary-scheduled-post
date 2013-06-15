package controllers

import play.api._
import libs.oauth._
import libs.oauth.ConsumerKey
import libs.oauth.OAuth
import libs.oauth.RequestToken
import libs.oauth.ServiceInfo
import libs.ws.WS
import play.api.mvc._
import scala.Left
import scala.Right

import libs.concurrent.Execution.Implicits._

object Application extends Controller {

  //HATENAでアプリケーションを登録した時に取得する値
  //動かす時はここに入力してください。
  val consumerKey = "Edjoo/C3KlnfFg=="
  val consumerSecret = "gAodYewEZZ133SFtAaf059qfHro="

  /*
   * indexページ。ログイン、ログアウトのメニューと、HATENA apiから情報を取得した結果を表示する。
   * TODO: そもそもトークンの中身をセッションとして持つのは微妙なので直せ
   */
  def index = Action {
    request => {

      val maybeAccessToken = request.session.get("token")
      val maybeAccessTokenSecret = request.session.get("secret")
      if (maybeAccessToken == None || maybeAccessTokenSecret == None)
        Ok(views.html.index("まだはてなにOAuthで認証していません。" +
          "<a href='/auth' >OAuthで認証する</a>"))
      else
      {
        //認証情報の作成
        val oauthCalculator = OAuthCalculator(
          ConsumerKey(consumerKey, consumerSecret),
          RequestToken(maybeAccessToken.get, maybeAccessTokenSecret.get)
        )

        // プロフィール情報の取得
        val url = "http://n.hatena.com/applications/my.json"
        Async {
          WS.url(oauthCalculator.sign(url)).get().map {
            response =>
              Ok(views.html.index(response.status + response.body))
          }
        }
      }
    }
  }

  /*
   * セッションを破棄してindexページにリダイレクト
   */
  def logout = Action {
    Redirect(routes.Application.index).withNewSession
  }

  /*
   * HATENA認証系のテスト
   * ここからほぼコピーしています　https://github.com/playframework-ja/Play20/wiki/ScalaOAuth
   */
  val KEY = ConsumerKey(consumerKey, consumerSecret)

  val HATENA = OAuth(ServiceInfo(
    "https://www.hatena.com/oauth/initiate?scope=read_public,write_public,read_private,write_private",
    "https://www.hatena.com/oauth/token",
    "https://www.hatena.ne.jp/oauth/authorize", KEY),
    true)

  def authenticate = Action {
    request =>
      request.queryString.get("oauth_verifier").flatMap(_.headOption).map {
        verifier =>
          val tokenPair = sessionTokenPair(request).get
          // We got the verifier; now get the access token, store it and back to index
          println("認証されました。アクセストークンを取得し、保存し、indexに戻ります")
          HATENA.retrieveAccessToken(tokenPair, verifier) match {
            case Right(t) => {
              // We received the authorized tokens in the OAuth object - store it before we proceed
              println("Oauthオブジェクトからアクセストークンを受け取りました。それを保存します。")
              Redirect(routes.Application.index).withSession("token" -> t.token, "secret" -> t.secret)
            }
            case Left(e) => throw e
          }
      }.getOrElse(
        HATENA.retrieveRequestToken("http://" + request.host + "/auth") match {
          //コールバックURL
          case Right(t) => {
            // We received the unauthorized tokens in the OAuth object - store it before we proceed
            println("認証されてないトークンを受け取りました。それを保存します。")
            Redirect(HATENA.redirectUrl(t.token)).withSession("token" -> t.token, "secret" -> t.secret)
          }
          case Left(e) => throw e
        })
  }

  def sessionTokenPair(implicit request: RequestHeader): Option[RequestToken] = {
    for {
      token <- request.session.get("token")
      secret <- request.session.get("secret")
    } yield {
      RequestToken(token, secret)
    }
  }


}