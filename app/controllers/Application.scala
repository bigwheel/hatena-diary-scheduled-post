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
   */
  def index = Action {
    request => {

      //セッションの中身の確認
      val accessToken = request.session.get("token").map {
        token =>
          println("token : " + token)
          token
      }.getOrElse {
        println("cannot take token")
        ""
      }
      val accessTokenSecret = request.session.get("secret").map {
        secret =>
          println("secret : " + secret)
          secret
      }.getOrElse {
        println("cannot take secret")
        ""
      }

      //ログイン判定
      // TODO こんな方法はダメなのでは。
      if (!(accessToken == "" && accessTokenSecret == "")) {
        println("log inning")
      } else {
        println("log outing")
      }

      //認証情報の作成
      val oauthCalculator = OAuthCalculator(ConsumerKey(consumerKey, consumerSecret), RequestToken(accessToken, accessTokenSecret))

      //タイムラインの取得
      val url = "http://n.hatena.com/applications/my.json"
      Async {
        //val a = WS.url(url).sign(oauthCalculator)
        val a = WS.url(oauthCalculator.sign(url)).withHeaders()
        println(a)
        a.get().map {
          response =>
            Ok(views.html.index(response.status + response.body))
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
        HATENA.retrieveRequestToken("http://localhost:9000/auth") match {
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