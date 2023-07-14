This repository includes the code which was used for this publication:  
  
**Schaffer, M., Leiria, D., Vera-Valdés, J. E., & Marszal-Pomianowska, A. (2023). Increasing the accuracy of low-resolution commercial smart heat meter data and analysing its error. Proceedings of the 2023 European Conference on Computing in Construction and the 40th International CIB W78 Conference, 4. https://doi.org/10.35490/EC3.2023.208**  
  
If you use this code, please cite the above, mentioned publication. 
## Indented aim
This code was used for all analysis, the algorithm presented in the above-mentioned paper. It includes the following steps:
1.	Prepare the data by imputing missing values and splitting he data at gaps longer than 48 values which are not imputed. 
2.	Analyses the error introduced by commercial smart heat meters by rounding down the transmitted energy values. 
3.	Test different settings of the proposed algorithm to reduce the rounding error.
4.	Analyse the results of the proposed algorithm with the optimal settings 
## Processing steps
The code files are supposed to be run in the numbered order.
## Important notice:
The data used in the before mentioned publication is not included in the repository as the authors have not the right to distribute the data. The code expects as input as .csv file with the following layout:
| ID | Time | Total |
| ------------- | ------------- | ------------- |
| Unique ID for each building | Hourly timestamps for every meter reading| Total recoded energy use for the respective hour in Wh | 
## Background
Hourly energy data from commercial smart heat meter data is commonly transmitted rounded down to whole kilowatt hour values. This reduced the usability of the data drastically. Thus analysing the error and developing a method to mitigate the error and therefore increase the usability of the data is important for a wide range of advanced analyses.
## BibTeX Citation
```
@inproceedings{EC32023_208,
	doi = {10.35490/EC3.2023.208},
	url = {https://ec-3.org/publications/conference/paper/?id=EC32023_208},
	year = {2023},
	month = {July},
	publisher = {European Council on Computing in Construction},
	author = {Markus  Schaffer  and  Daniel  Leiria  and  J. Eduardo  Vera-Valdés  and  Anna  Marszal-Pomianowska},
	title  = {Increasing the accuracy of low-resolution commercial smart heat meter data and analysing its error},
	booktitle = {Proceedings of the 2023 European Conference on Computing in Construction and the 40th International CIB W78 Conference},
	volume  = {4},
	isbn = {},
	address  = {Crete, Greece},
	series  = {Computing in Construction},
	language = {en-GB},
	abstract = {Recent research has demonstrated the fundamental potential of smart heat meter (SHM) data. However, it has also been shown that the usability of the data is reduced because SHM energy measurements are commonly rounded down (truncated) to kilowatt-hour values. This study therefore investigates, for the first time, the error introduced by truncation using a high-resolution dataset. Furthermore, a method is developed to reduce the loss of information in the truncated data by combining smoothing with a ruleset and scaling approach (SMPS). SMPS is shown to increase the pointwise accuracy and correlation of the truncated data with the full-resolution data.},
	issn = {2684-1150},
	Organisation = {European Conference on Computing in Construction},
	Editors = {}
}
```
