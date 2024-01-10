Order of operations for upload:

-   Manuale Lovaniense (ODIS)

-   Magister Dixit (MARC)

-   Lovaniensia (Omeka CSV)

## Manuale Lovaniense

-   Get next filemaker IDs for Sources and Places table, enter in script.

-   Get list of all place strings used, export to Googlesheets. Manually enter place IDs from filemaker.

-   For those missing places, enter Wikidata ID.

-   Import this file back and run script to get details from Wikidata for those missing.

-   Upload these to Places table, assign FM place IDs based on next one

-   Create PlacesLinks table for these too

-   Merge found and missing into a full place concordance

-   Get tables with basic info (date, language etc.) from MARC. Join with places concordance.

-   Create Sources table from all info

-   Create new sourceLinks with USTC etc. values. Merge with ODIS links (found in Source table)

-   Upload Sources table

-   Upload SourceLinks table

-   Make concordance of MMS IDs and new FM IDs to sent to Yanne

## Magister Dixit

-   Get next Sources ID (only after previous upload is completed).

-   Make institutions concordance - fill in manually from FM or create new ones

-   Get all details from MARC and make sources table.

-   Create LIBIS.be as a project, note down ID

-   Use 856 field to get libis links, create sourceLinks table, use new project ID.

-   Upload Sources table

-   Upload sourcesLinks table

-   Make concordance of MMS IDs and new FM IDs to sent to Yanne

## Lovaniensia

Get next Sources and Places FM IDs (only after previous uploads are completed)

Make list of place name strings, upload to Google sheets. Manually add FM IDs (only after all new FM place IDs have been made in the previous step). Add wikidata IDs for any missing places

Import to R. Run function to get details from Wikidata for missing. Upload as new Places to FM. Merge found and missing to create concordance.

Create PlacesLinks table for missing places - add geonames and Wikidata links. Enter correct project numbers. Upload places and PlaceLinks

Create sources table from .csv

Create two sourceLinks tables, one from .csv (with the Lovaniensia URIs) and the other from the MARC 856 field.

Upload sources and sourcesLinks tables.
