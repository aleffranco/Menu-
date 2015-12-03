--cadastros:
--https://68df48a6-917b-4105-bca0-2f51cb9b93bf-app.fpcomplete.com/ordem
--https://68df48a6-917b-4105-bca0-2f51cb9b93bf-app.fpcomplete.com/prod
--https://68df48a6-917b-4105-bca0-2f51cb9b93bf-app.fpcomplete.com/supermercado

--Listas:
--https://68df48a6-917b-4105-bca0-2f51cb9b93bf-app.fpcomplete.com/
--https://68df48a6-917b-4105-bca0-2f51cb9b93bf-app.fpcomplete.com/autor
--https://68df48a6-917b-4105-bca0-2f51cb9b93bf-app.fpcomplete.com/listsuper
--https://68df48a6-917b-4105-bca0-2f51cb9b93bf-app.fpcomplete.com/listprod
--https://68df48a6-917b-4105-bca0-2f51cb9b93bf-app.fpcomplete.com/resultado

--codigo fonte:
{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleContexts,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, ViewPatterns #-}
module Pesquisa where

import Database.Persist.Postgresql
import Control.Monad.Logger (runStdoutLoggingT)
import Data.Text (Text)
import Data.Time -- dia hora
import qualified Data.Text as T
import Control.Applicative
import Yesod
import Yesod.Core
import Text.Hamlet
import Text.Cassius
import Text.Lucius
import Text.Julius

--import qualified Database.Esqueleto as E
--import Database.Esqueleto ((^.))
data Pesquisar = Pesquisar{connPool :: ConnectionPool}

instance Yesod Pesquisar

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Supermercado
   nome Text
   deriving Show
Produto
   nome Text sqltype=varchar(20)
   descricao Text
   estoque Int
   precoAnter Double
   precoAtual Double 2
Ordem
   fornId SupermercadoId
   pecaId ProdutoId -- chave extrangeira  
   qtde Int
   data UTCTime default=now() -- data hora dia pega a hora do sistema (importa datatime)
   processado Bool
   UniqueSP fornId pecaId -- id 1,2,3, superecId 1,3,4 prod 1,2,3 join
|]

mkYesod "Pesquisar" [parseRoutes|
  /prod ProdutoR GET POST
  /supermercado SuperR GET POST
  /listprod ListarProdutoR GET
  /listsuper ListarSuperR GET
  /ordem OrdemR GET POST
  / ListarOrdemR GET
  /autor AutorR GET
  /menu MenuR GET
  /resultado ResultadoR GET
|]

instance YesodPersist Pesquisar where
   type YesodPersistBackend Pesquisar = SqlBackend
   runDB f = do
       master <- getYesod
       let pool = connPool master
       runSqlPool f pool

type Form a = Html -> MForm Handler (FormResult a, Widget)

instance RenderMessage Pesquisar FormMessage where
    renderMessage _ _ = defaultFormMessage

formOrdem :: Form Ordem
formOrdem = renderDivs $ Ordem <$>
             areq (selectField supers) "Supermercado:" Nothing <*>
             areq (selectField prods)  "Escolha Prod.:" Nothing <*>
             areq intField             "Qtde Produto:" Nothing <*>
             lift (liftIO getCurrentTime) <*> -- dia atual do cadastro liftIO funçao monad valor fixo no formulario
             lift (liftIO $ return False)

prods = do
       entidades <- runDB $ selectList [] [Asc ProdutoNome] 
       optionsPairs $ fmap (\ent -> (produtoNome $ entityVal ent, entityKey ent)) entidades

supers = do
       entidades <- runDB $ selectList [] [Asc SupermercadoNome] 
       optionsPairs $ fmap (\ent -> (supermercadoNome $ entityVal ent, entityKey ent)) entidades

formProduto :: Form Produto
formProduto = renderDivs $ Produto <$>
             areq textField   "Nome do Produt" Nothing <*>
             areq textField   "Descrição Prod." Nothing <*>
             areq intField    "Quantidad Prod." Nothing <*>
             areq doubleField "Preço Anterior." Nothing <*>
             areq doubleField "Prco Pesquisado" Nothing

formSuper :: Form Supermercado
formSuper = renderDivs $ Supermercado <$>
             areq textField "Nome do Supermercado..:" Nothing

widgetForm :: Route Pesquisar -> Enctype -> Widget -> Text -> Widget
widgetForm x enctype widget y = [whamlet|
            <body style="background-color:LavenderBlush">
            <h1 style="color: DeepSkyBlue;"> Cadastro: #{y}
            
            <form method=post action=@{x} enctype=#{enctype}>
                ^{widget}
                <input type="submit" value="Enter">
|]

getProdutoR :: Handler Html
getProdutoR = do
           (widget, enctype) <- generateFormPost formProduto
           defaultLayout $ widgetForm ProdutoR enctype widget "Produto"

postProdutoR :: Handler Html
postProdutoR = do
            ((result,_),_) <- runFormPost formProduto
            case result of
                FormSuccess prod -> (runDB $ insert prod) >> defaultLayout [whamlet|
                <body style="background-color:LavenderBlush">
                  <h1 style="color: DeepSkyBlue;"> Produto inserido|]
                _ -> redirect ProdutoR


getSuperR :: Handler Html
getSuperR = do
           (widget, enctype) <- generateFormPost formSuper
           defaultLayout $ widgetForm SuperR enctype widget "Supermercado"

postSuperR :: Handler Html
postSuperR = do
            ((result,_),_) <- runFormPost formSuper
            case result of
                FormSuccess super -> (runDB $ insert super) >> defaultLayout [whamlet|
                <body style="background-color:LavenderBlush">
                  <h1 style="color: DeepSkyBlue;"> Supermercado inserido|]
                _ -> redirect SuperR

getListarProdutoR :: Handler Html
getListarProdutoR = do
                 produtos <- runDB $ selectList [] [Asc ProdutoNome]
                 defaultLayout [whamlet|
                     <body style="background-color:LavenderBlush">
                      <h1 style="color: DeepSkyBlue;"> Produtos Cadastrados:
                        <style>
                          table, th, td { border: 1px solid black;}
                      <table>
                        <td>
                            Lista de Produtos
                          $forall Entity pid prod <- produtos
                            <tr><td style="color: green;">#{produtoNome prod}
                            <tr><td>Prc.Anterior:#{produtoPrecoAnter prod}
                            <tr><td>Prc.Atual...:#{produtoPrecoAtual prod}
                 |]
-- <h5> #{produtoDescricao prod}

getListarSuperR :: Handler Html
getListarSuperR = do
                 superm <- runDB $ selectList [] [Asc SupermercadoNome]
                 defaultLayout [whamlet|
                     <body style="background-color:LavenderBlush">
                      <h1 style="color: DeepSkyBlue;"> Supermermercados Cadastrados:
                        <style>
                          table, th, td { border: 1px solid black;}
                      <table>
                        <tr>
                            Supermercados:
                       $forall Entity fid super <- superm
                         <td> #{supermercadoNome super}
                 |]

getOrdemR :: Handler Html
getOrdemR = do
           (widget, enctype) <- generateFormPost formOrdem
           defaultLayout $ widgetForm OrdemR enctype widget "Pesquisa de preços."

postOrdemR :: Handler Html
postOrdemR = do
            ((result,_),_) <- runFormPost formOrdem
            case result of
                FormSuccess x -> (runDB $ insert x) >> defaultLayout [whamlet|<h1> Pesquisa de preços inserida com sucessos!|]
                _ -> redirect OrdemR

getResultadoR :: Handler Html
getResultadoR = do
                 ordens <- runDB $ (rawSql "SELECT ??, ??, ?? \
                                           \FROM ordem \
                                           \INNER JOIN produto       ON ordem.peca_id=produto.id \
                                           \INNER JOIN supermercado ON ordem.peca_id=supermercado.id "
                                           [])::Handler [(Entity Ordem, Entity Produto, Entity Supermercado)]
                 defaultLayout [whamlet|
                      <body style="background-color:LavenderBlush">
                        <h1 style="color: DeepSkyBlue;"> Produtos Pesquisados:
                          <style>
                              table, th, td {
                                border: 1px solid black;
                              }
                        <table>
                         <tr>
                             <td>Data....:<td>Nro.Item:<td>Supermercado:<td>Quantidade:<td>Produtos:<td>Preço Anterior:<td>Preço Atual:
                                $forall (Entity oq ordem, Entity _ prd, Entity _ nf) <- ordens
                                 <tr>
                                 <td style="color: green; "> #{show $ utctDay $ ordemData ordem}
                                 <td style="color: orange;">#{fromSqlKey oq}
                                 <td style="color: blue;  ">#{supermercadoNome nf}
                                 <td style="color: purple;">#{show $ ordemQtde  ordem}
                                 <td style="color: black; "><b>#{produtoNome prd}
                                 <td style="color: DarkRed; ">#{produtoPrecoAnter prd}
                                 <td style="color: red;   ">#{produtoPrecoAtual prd}
                 |]



getAutorR :: Handler Html
getAutorR = defaultLayout [whamlet|
              <body style="background-color:LavenderBlush">
                <h1 style="color: DeepSkyBlue;"> Autores:
                <h3> Alef Rodrigues Franco <br>
                     Isabela Baraldi Gandelman <br>
                     Osvaldo Pereira Cotrim <br>
|]

ww :: Widget
ww = toWidgetHead [hamlet|
<link rel="author" href=@{AutorR} title="Sobre...">
|]

getMenuR :: Handler Html
getMenuR = defaultLayout [whamlet| <h1>
|]

getListarOrdemR :: Handler Html
getListarOrdemR = defaultLayout [whamlet| <h1>
                   <body style="background-color:LavenderBlush">
                    <div class="divNmPagina"><center><b>Menu - Pesquisa de Preços
                      <h4 style="color: DeepSkyBlue;"> Opções:<h5> 
                       <a href= @{MenuR}>
                        <ul>        "
                        <li> Cadastra Produtos <br> 
                        <li> Cadastra Pesquisa <br>
                        <li> Cadastra Supermercados<br>
                        <li> Lista os Autores <br>
                        <li> Lista de Produtos <br>
                        <li>Lista de Resultado <br>
                        <li> Lista de Supermercados <br>
#menu ul{
    width: 300px;
    height: auto;
    background-color: #2f4f4f                                                     
    display: block;
    margin: 0;                                                    
    padding: 0;
    width: 306px;                                                      
}

#menu li{
    list-style: none;
    display: block;                                                        
    width: 100%;
    height: auto;                                                        
    border-bottom: 2px solid #fff;
    color: #fffafa;
    font-family: helvetica;
}

#menu li:hover{
    background: #ff4500;
    width: 266px;                                                  
}

#menu li{
    -webkit-transition: all .9s ease;
    -moz-transition: all .9s ease;
    -ms-transition:all .9s ease;
    transition: all .9s ease;
}

h1{
    font-famiçy: helvetica;
    color: #8fbc8f                                                     
                                                        }                                                        

|]

connStr = "dbname=d266oucgqg58dl host=ec2-107-21-224-11.compute-1.amazonaws.com user=fmtteyovbsxaip password=2henlXgzObUw18mREZh_TjF4rN port=5432"

main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do
       runSqlPersistMPool (runMigration migrateAll) pool
       warpEnv (Pesquisar pool)
