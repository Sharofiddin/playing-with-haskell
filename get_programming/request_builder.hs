getRequestUrl host apiKey resource id = host ++ "/" ++ resource ++ "/" ++ id ++ "?token=" ++ apiKey

genHostRequestBuilder host = (\apiKey resource id ->
  getRequestUrl host apiKey resource id)

exampleUrlBuilder = genHostRequestBuilder "http://example.com"

