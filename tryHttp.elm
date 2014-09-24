module TryHttp where

--
-- NB This produces Failure due I think to cross-site-scripting security issues.
-- It should work when the http request is to the site that hosts the page.
--

import Http as H

someUrl = "http://www.google.co.uk/"

display response = 
  case response of
    H.Success a -> asText a
    H.Waiting -> plainText "Waiting"
    H.Failure code msg -> plainText <| "Failure " ++ (show code) ++ msg   

main = lift display (H.sendGet <| constant someUrl)
