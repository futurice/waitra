# Waitra

Waitra is a very simple router.
It's useful for writing simple API web-services,
when you don't want to use the whole Yesod stack.

[![Build Status](https://travis-ci.org/futurice/waitra.svg?branch=master)](https://travis-ci.org/futurice/waitra)

## Synopsis

```hs
echoRoute :: Route
echoRoute = routeGet (string "/api/echo/" *> many anySym) echoApp
  where echoApp msg _req respond = respond $ responseLBS status200 [] (fromString msg)

app :: Application
app = waitraMiddleware [echoRoute] fallbackApp
```
