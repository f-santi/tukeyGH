
## Answers to the issues raised on the previous submission


> If there are references describing the methods in your package, please
> add these in the description field of your DESCRIPTION file in the form
> authors (year) <doi:...>
> authors (year) <arXiv:...>
> authors (year, ISBN:...)
> or if those are not available: <[https:...]https:...>
> with no space after 'doi:', 'arXiv:', 'https:' and angle brackets for
> auto-linking.
> (If you want to add a title as well please put it in quotes: "Title")

The package implements some statistical methods which are proposed in various
articles and books, and all of them are equally important. Thus, there would be
two alternative options: (1) include a single reference, and in that case, the
field DESCRIPTION would be incomplete and misleading; (2) include all relevant
references, and in that case, the field DESCRIPTION would look more like a
messy bibliography.

This is the reason why the field DESCRIPTION is unchanged.



> \dontrun{} should only be used if the example really cannot be executed
> (e.g. because of missing additional software, missing API keys, ...) by
> the user. That's why wrapping examples in \dontrun{} adds the comment
> ('# Not run:') as a warning for the user.
> Does not seem necessary.

> Please unwrap the examples if they are executable in < 5 sec, or replace
> \dontrun{} with \donttest{}.

That example is wrapped by \dontrun{} because that example takes more than
**30 minutes** to be run. I added an explicit warning as a comment to the
example.





## Test environments


- Ubuntu Linux 20.04 LTS (focal), R 4.0.5:

  * checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Flavio Santi <flavio.santi@univr.it>'

  New submission

  0 errors | 0 warnings | 1 note



- Windows (winbuilder):

  devel:        0 errors | 0 warnings | 1 note

  * checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Flavio Santi <flavio.santi@univr.it>'

  New submission

  release:      0 errors | 0 warnings | 1 note

    * checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Flavio Santi <flavio.santi@univr.it>'

  New submission



- macOS 10.13.6 High Sierra, R-release, brew

  * checking dependencies in R code ... NOTE
  Namespace in Imports field not imported from: ‘Rdpack’
    All declared Imports should be used.

  0 errors | 0 warnings | 1 note



- macOS 10.13.6 High Sierra, R-release, CRAN's setup

  * checking dependencies in R code ... NOTE
  Namespace in Imports field not imported from: ‘Rdpack’
    All declared Imports should be used.

  0 errors | 0 warnings | 1 note



- Windows Server 2008 R2 SP1, R-release, 32/64 bit

  * checking dependencies in R code ... NOTE
  Namespace in Imports field not imported from: ‘Rdpack’
    All declared Imports should be used.

  0 errors | 0 warnings | 1 note



- Oracle Solaris 10, x86, 32 bit, R-release
  
  0 errors | 0 warnings | 1 note

  * checking dependencies in R code ... NOTE
  Namespace in Imports field not imported from: ‘Rdpack’
    All declared Imports should be used.



