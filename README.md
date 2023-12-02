# ivolR

### Set up

Package can be installed by running:

```
remotes::install_github("bt-koch/ivolR")
```

First, declare API key as environment variable names `IVOLATILITY_API_KEY` in
`.Renviron`. You can do this manually or use utility from `ivolR`:

```
ivolR::set_apikey(your_key)
```

This will automatically save your API key in the `.Renviron` (and creates one
if it not yet exists) and by default restarts `R`.

Now everything is set up to request data from the [ivolatility.com](https://www.ivolatility.com/landing/)
API via the `ivolR::request()` function.

### Example usage

Please consult the [API documentation](https://redocly.github.io/redoc/?nocors&url=https://restapi.ivolatility.com/api-docs#section/Introduction)
to see which endpoints are available and which parameters are required and optional.

The endpoints can be requested by running:

```
ivolR::request(endpoint = "/equities/underlying-info", symbol="AAPL")
```

Please consult function documentation to see furhter options to modify the
request.
