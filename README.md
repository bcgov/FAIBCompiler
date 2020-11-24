<!-- Add a project state badge
See https://github.com/BCDevExchange/Our-Project-Docs/blob/master/discussion/projectstates.md
If you have bcgovr installed and you use RStudio, click the 'Insert BCDevex Badge' Addin. -->

FAIBCompiler
============

FAIBCompiler comtains the functions to run VRI and PSP compilers. The
compilers

-   load data from FAIB oracle ground sample databases using package
    **FAIBOracle**,
-   manipulate tree/stand-level tables, calculate tree/stand-level
    attributes such as basal area and volume using **FAIBBase**,
-   save and archive them.

### Features

The branch VGISCompiler saves the previous version of VRICompiler and
first version of ISMCCompiler on Nov. 24, 2020. The version freezes
those two compilers due to a major change of cluster id. Specifically,
the formation cluster id in this branch is formated as
proj\_id-samp\_no-samp\_type-visit, while in the future development, the
cluster id will be formated as site\_identifier-samp\_type-visit. The
unique spatial sample point also changed due to change of ISMC EFR, from
proj\_id-samp\_no in current branch to site\_identifier in the future
compiler.

### Installation

### Usage

#### Example

This is a basic example which shows you how to solve a common problem:

``` r
## basic example code
```

### Project Status

### Getting Help or Reporting an Issue

To report bugs/issues/feature requests, please file an
[issue](https://github.com/bcgov/FAIBCompiler/issues/).

### How to Contribute

If you would like to contribute to the package, please see our
[CONTRIBUTING](CONTRIBUTING.md) guidelines.

Please note that this project is released with a [Contributor Code of
Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree
to abide by its terms.

### License

    Copyright 2019 Province of British Columbia

    Licensed under the Apache License, Version 2.0 (the &quot;License&quot;);
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an &quot;AS IS&quot; BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

------------------------------------------------------------------------

*This project was created using the
[bcgovr](https://github.com/bcgov/bcgovr) package.*
