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
import play.api.libs.json.{JsObject, Json}

object Application extends Controller {

  //HATENAでアプリケーションを登録した時に取得する値
  //動かす時はここに入力してください。
  val consumerKey = "Edjoo/C3KlnfFg=="
  val consumerSecret = "gAodYewEZZ133SFtAaf059qfHro="

  def requestTokenFromSession(request: Request[AnyContent]): Option[RequestToken] = {
    val maybeAccessToken = request.session.get("token")
    val maybeAccessTokenSecret = request.session.get("secret")

    if (maybeAccessToken == None || maybeAccessTokenSecret == None)
      None
    else
      Some(RequestToken(maybeAccessToken.get, maybeAccessTokenSecret.get))
  }

  def getOauthCalculator(request: Request[AnyContent]) = OAuthCalculator(
    ConsumerKey(consumerKey, consumerSecret),
    requestTokenFromSession(request).get
  )


  /*
   * indexページ。ログイン、ログアウトのメニューと、HATENA apiから情報を取得した結果を表示する。
   * TODO: そもそもトークンの中身をセッションとして持つのは微妙なので直せ
   */
  def index = Action {
    request => {

      val maybeRequestToken = requestTokenFromSession(request)
      if (maybeRequestToken == None)
        Ok(views.html.index("まだはてなにOAuthで認証していません。" +
          "<a href='/auth' >OAuthで認証する</a>"))
      else
      {
        //認証情報の作成
        val oauthCalculator = getOauthCalculator(request)

        // プロフィール情報の取得
        val url = "http://n.hatena.com/applications/my.json"
        Async {
          WS.url(oauthCalculator.sign(url)).get().map {
            response =>
              val json = Json.parse(response.body)
              val url_name = (json  \ "url_name").as[String]
              val url = "http://d.hatena.ne.jp/" + url_name + "/atom/draft"
              Async {
                WS.url(oauthCalculator.sign(url)).get().map {
                  response =>
                    val result = (response.xml \\ "entry").toList.map (
                      entry => {
                        val url = (entry \ "link" \ "@href").toString
                        val matches = "http://d\\.hatena\\.ne\\.jp/(.+)/atom/draft/(\\d+)".r.findFirstMatchIn(url)
                        val url_name = matches.get.group(1)
                        val article_id = matches.get.group(2)
                        "<a href='" + "http://d.hatena.ne.jp/" + url_name + "/draft?epoch=" + article_id + "'>" + (entry \ "title").text + "</a>"
                      }
                    )
                      Ok(views.html.index(result.mkString("<BR />")))
                }
              }
          }
        }
      }
    }
  }

  def post(url_name: String, article_id: Int) = Action {
    request => {
      val url = "http://d.hatena.ne.jp/" + url_name + "/atom/draft/" + article_id.toString
      Async {
        WS.url(getOauthCalculator(request).sign(url)).withHeaders("X-HATENA-PUBLISH" -> "1").put(Results.EmptyContent()).map {
          response =>
            Ok(response.status + response.body + url)
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