## Response to reviews

- Have [the] issues [that caused the original package to be archived] been solved?
Yes, as far as I know the package was archived because the native routines were not properly registered. This issue has been remedied as of version 2.1-13.

- As requested, the Description was modified to use the Authors@R field and acronyms were removed from the package description. I also added two references to the package description.

- The LICENSE file was removed since it was redundant with the license stated in the package description.

- Functions no longer use the `cat` command to write messages to the console; `message`, `warning` or `stop` (as appropriate) are now used instead.

- Examples were corrected so that they reset the user's par settings after running.

## R CMD check results

0 errors | 0 warnings | 1 note

* New submission

- While this is marked as a new submission, it was in fact [published on CRAN previously](https://cran.r-project.org/web/packages/VPdtw/index.html).
VPdtw was removed from CRAN on 2017-03-13 -- apparently, because the
"native routines" were not properly registered. This issue has been remedied with
this new version. I am taking over from David Clifford as maintainer with his 
permission.

- Found the following (possibly) invalid DOIs:
  DOI: 10.1021/ac802041e
    From: DESCRIPTION
    Status: Service Unavailable
    Message: 503
    
^ This is a valid doi.