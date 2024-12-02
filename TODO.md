# Wuebutab / Tabbouleh Development To-Do

## Short term (1.0): Deliver functional, tested and clean CLI app

* Refactor tables:
  * speeches
  * names
  * speakers
  * results
* Change Actions into a class called "TabController" or similar
* Track pull-ups by round rather than as a total
* Add panel gender balance as a weighted constraint in judge allocation
* Add clean error messages:
  * unfindable/misnamed table fields
  * Missing structure tab 
* Check for orphaned code from old functionality
* Test all functionality
* Cross-check results with a manual shadow tab
* Document existing code
* Refactor name to Tabbouleh, release CLI version

## Medium term (2.0): Develop web interface

* Change Auth to only request access for a single spreadsheet, if possible
* Build REST API
* Build Frontend

## Long term (2.1...): Add non-essential features

* Add stats module
* Implement cross-tournament rating system, including pre-Tabbouleh data
