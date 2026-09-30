# Regional Remote Sensing Clarity Model

Primary repository contact: B Steele

Repository containing the regional clarity model using Landsat Collection 2 SR product data via AquaSat v2 data products (siteSR, lakeSR, )

This repository is covered by the MIT use license. We request that all downstream uses of this work be available to the public when possible.

## Secrets/credentials

API keys and other credentials are stored locally in a `.Renviron` file (untracked by git). This workflow requires an API key for accessing the
EDI data repository. Copy `.Renviron.example` to `.Renviron` and fill in real values. R loads `.Renviron` automatically at session start; access 
values in code with `Sys.getenv("KEY_NAME")`.
