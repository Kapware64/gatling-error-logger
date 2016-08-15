Gatling Error Logger
=========================

This is a Play Framework server that logs failed requests from Gatling simulations and graphs them by date (with entries sorted by number of error occurrences). It utilizes D3.js to graph, Skeleton (http://getskeleton.com/) for the front-end appearance, and Slick for the database.

Quickstart Guide
=========================

Get the project
---------------

```bash
git clone https://github.com/Kapware64/gatling-error-logger.git
```

Configure Admin Passowrd
------------------------
In application.conf (location in the conf folder), change the "adminKey" field to your own admin password.

Run the project
---------------
* You can run this project locally as a Play 2 App. Here are JVM options for doing this when running on http://localhost:9000: "-Xms512M -Xmx4096M -Xss20M -XX:+CMSClassUnloadingEnabled -XX:MaxPermSize=256M." In IntelliJ, these configurations are done in the "Run/Debug Configurations" menu.
* To deploy this application, see https://www.playframework.com/documentation/2.5.x/Production.

Logging Errors
--------------
Errors are logged via POST requests to "[URL you are running the server on]/error" with request bodies containing json in this format:
```json
  {
      "key": "[YOUR ERROR LOGGING SERVER'S ADMIN KEY]",
      "content": [{
          "name": "viewPlacementsRequest",
          "date": "8-11-2016"
      }, {
          "name": "viewPlacementsRequest",
          "date": "8-11-2016"
      }]
  }
```
You can find a Gatling simulation which automatically finds the latest recorded simulation log, converts it to this format, and makes the POST request here: https://github.com/Kapware64/stress-tester/blob/master/src/test/scala/com/medialets/LogErrors.scala.
