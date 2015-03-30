# PolitwoopsR
Extract deleted tweet and politician data from the Politwoops project.

<a href="http://politwoops.com"  target="_blank">Politwoops</a> is an international project that tracks politicians on Twitter and records their deleted tweets. The project started in the Netherlands and spread to dozens of other countries. In the US, it is run by the <a href="http://sunlightfoundation.com/"  target="_blank">Sunlight Foundation</a>. 

The PolitwoopsR package includes functions that extract tweet data from <strike>any</strike> some <a href="http://politwoops.com/"  target="_blank">Politwoops</a> projects. It also provides a way to scrape politician data from the <a href="http://politwoops.sunlightfoundation.com"  target="_blank">US Politwoops project</a>.

NOTE: The package has been tested mostly on the US project. I will be updating it to make sure it runs smoothly for other countries, but for now there might be bugs in cases where the URL or json data structure are formed in an unusual way.

To install the package, you need devtools:
  <p><strong>install.packages('devtools')<br> 
  devtools::install_github('kateto/PolitwoopsR')</strong></p>

Some visualizations based on politwoops data are <a href="http://kateto.net/politwoops" target="_blank">available here</a>.

<a href="http://kateto.net/politwoops" target="_blank">
<img src="http://kateto.net/wordpress/wp-content/uploads/2015/03/Politwoops-All-Charts-2015.png" width="600"> </a>
