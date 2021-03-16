module Server where

import Servant
import Lucid
import Servant.HTML.Lucid (HTML(..))

app :: Application
app = serve apiProxy api

apiProxy :: Proxy Api
apiProxy = Proxy

type Api = MainPage :<|> BlogPost
type MainPage = Get '[HTML] (Html ())
type BlogPost = "blog" :> QueryParam "id" BlogId :> Get '[HTML] (Html ())

type BlogId = Integer

api :: Server Api
api = mainPage :<|> blogPost

mainPage :: Handler (Html ())
mainPage = pure ()

blogPost :: BlogId -> Handler (Html ())
blogPost blogId = pure ()
