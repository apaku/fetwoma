-- Copyright (c) 2013, Andreas Pakulat <apaku@gmx.de>
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
--     * Redistributions of source code must retain the above copyright
--       notice, this list of conditions and the following disclaimer.
--     * Redistributions in binary form must reproduce the above copyright
--       notice, this list of conditions and the following disclaimer in the
--       documentation and/or other materials provided with the distribution.
--     * Neither the name of the <organization> nor the
--       names of its contributors may be used to endorse or promote products
--       derived from this software without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
-- ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
-- WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
-- DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
-- (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
-- LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
-- ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
-- (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
-- SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
module Main where

import Network.Curl.Download
import Text.Feed.Query
import Text.Feed.Types
import Network.Mail.Mime
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Maybe

justMailAddress :: String -> Address
justMailAddress addr = Address {addressName = Nothing, addressEmail = T.pack addr}

mailAddressAndName :: String -> String -> Address
mailAddressAndName name email = Address {addressName = Just $ T.pack name, addressEmail = T.pack email}

maybeAsText :: (a -> Maybe String) -> a -> T.Text
maybeAsText fn entry = T.pack $ fromMaybe "" $ fn entry

generateMailHeadersFromFeedEntry :: Feed -> Item -> [(B.ByteString, T.Text)]
generateMailHeadersFromFeedEntry feed entry = [(C8.pack "Subject", maybeAsText getItemTitle entry),
                                               (C8.pack "X-RSS-URL", maybeAsText getItemLink entry),
                                               (C8.pack "X-RSS-Feed", maybeAsText getFeedHome feed),
                                               (C8.pack "X-RSS-ID", T.pack $ snd $ fromMaybe (False,"") (getItemId entry))]

createHtmlPart :: String -> Part
createHtmlPart content = Part { partType = T.pack "text/html",
                                partEncoding = QuotedPrintableText,
                                partFilename = Nothing,
                                partHeaders = [],
                                partContent = L8.pack content }

getFromName :: Feed -> Item -> String
getFromName feed entry = unwords [getFeedTitle feed, fromMaybe "" (getItemAuthor entry)]

generateMailFromEntry :: Feed -> Item -> Mail
generateMailFromEntry feed entry = Mail {mailFrom = mailAddressAndName (getFromName feed entry) "fetwoma@localhost",
                                         mailTo = [justMailAddress "andreas@localhost"],
                                         mailCc = [],
                                         mailBcc = [],
                                         mailHeaders = generateMailHeadersFromFeedEntry feed entry,
                                         mailParts = [ [ createHtmlPart $ fromMaybe "" $ getItemDescription entry ] ]}

main::IO()
main = do
    putStrLn "Feed to fetch:"
    feedurl <- getLine
    downloaded <- openAsFeed feedurl
    case downloaded of
        Left err -> putStrLn err
        Right feed -> do
            mails <- mapM (renderMail' . generateMailFromEntry feed) (getFeedItems feed)
            print mails
