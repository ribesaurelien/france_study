<!--[![DOI](https://zenodo.org/badge/398859517.svg)](https://zenodo.org/badge/latestdoi/398859517)-->



Ribes et al. (2022) routines
============================

A set of raw data, R routines and notebooks, to reproduce key figures from Ribes et al. (2022; see reference below). This is organized in three distinct steps: 
* estimation of forced responses in a set of CMIP6 models; see [here](https://gitlab.com/ribesaurelien/france_study/notebook_forced_response),
* implementation of the observational constraint (KCC); see [here](https://gitlab.com/ribesaurelien/france_study/notebook_constrain_figures),
* estimation of non-stationary climate normals from observations; see [here](https://gitlab.com/ribesaurelien/france_study/notebook_normals).

Content
-------

* [Related Publications](#related-publications)
* [Installation](#installation)
* [Contributors](#contributors)
* [Acknowledgments and Reference](#acknowledgments-and-reference)
* [License](#license)


Related Publications
--------------------

Ribes A., J. Boé, S. Qasmi, B. Dubuisson, H. Douville, L. Terray (2022) An updated assessment of past and future warming over France using regional observational constraints, submitted to ESD.

Qasmi, S. and Ribes, A. (in revision): Reducing uncertainty in local climate projections _Journal of Climate_ DOI:  <a href="https://doi.org/10.21203/rs.3.rs-364943/v1">10.21203/rs.3.rs-364943/v1</a>

Ribes, A. et al. (2021): Making climate projections conditional on historical observations. _Science Advances_, 7, eabc0671, DOI: <a href="https://doi.org/10.1126/sciadv.abc0671">10.1126/sciadv.abc0671</a>

Rigal A., J.-M. Azaïs, A. Ribes (2019) : Estimating daily climatological normals in a changing climate, Climate Dynamics, 53 (1–2), 275-286, doi:<a href="https://link.springer.com/article/10.1007/s00382-018-4584-6">10.1007/s00382-018-4584-6</a>



Installation
------------

These routines use the [KCC R package](https://gitlab.com/saidqasmi/KCC)

KCC needs R version 3.5.3 or higher. If needed, install the <code>devtools</code> package in a R console:
<code>install.packages("devtools")</code>

Download and install the KCC package with the following line: 
<code>install_git("https://gitlab.com/saidqasmi/KCC.git")</code>

Contributors
------------

- Aurélien Ribes (aurelien.ribes@meteo.fr)
- Saïd Qasmi (said.qasmi@meteo.fr)
- Alix Rigal

Acknowledgments and Reference
-----------------------------

If you use our routines, please cite the corresponding paper.

License
-------

These routines are free: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.

