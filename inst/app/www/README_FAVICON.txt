Favicon Instructions
====================

To add a custom favicon to your application:

1. Create or obtain a favicon file (favicon.ico)
   - Recommended size: 16x16 or 32x32 pixels
   - You can generate one at: https://favicon.io/

2. Place the favicon.ico file in this directory:
   inst/app/www/favicon.ico

3. The favicon will automatically be loaded by golem's bundle_resources()
   function in R/app_ui.R

4. Clear your browser cache to see the new favicon

Alternative: You can also use PNG format
   - Save as: inst/app/www/favicon.png
   - Update R/app_ui.R to use: favicon(ico = "www/favicon.png")