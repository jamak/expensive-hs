module Handler.Transaction (
  getExpenseR,
  postExpenseR,
  getTransactionR)
where

import Import
import Data.Monoid

instance YesodNic App

entryForm :: Form Transaction
entryForm = renderDivs $ Transaction
    <$> areq   textField "Title" Nothing
    <*> areq   nicHtmlField "Content" Nothing

-- The view showing the list of articles
getExpenseR :: Handler RepHtml
getExpenseR = do
    -- Get the list of transactions inside the database.
    transactions <- runDB $ selectList [] [Desc transactionTitle]
    -- We'll need the two "objects": transactionWidget and enctype
    -- to construct the form (see templates/transactions.hamlet).
    (transactionWidget, enctype) <- generateFormPost entryForm
    defaultLayout $ do
        $(widgetFile "transactions")

postExpenseR :: Handler RepHtml
postExpenseR = do
    ((res,transactionWidget),enctype) <- runFormPost entryForm
    case res of 
         FormSuccess transaction -> do 
            transactionId <- runDB $ insert transaction
            setMessage $ toHtml $ (transactionTitle transaction) <> " created"
            redirect $ TransactionR transactionId 
         _ -> defaultLayout $ do
                setTitle "Please correct your entry form"
                $(widgetFile "transactionAddError")

getTransactionR :: TransactionId -> Handler RepHtml
getTransactionR transactionId = do
    transaction <- runDB $ get404 transactionId
    defaultLayout $ do
        setTitle $ toHtml $ transactionTitle transaction
        $(widgetFile "transaction")
