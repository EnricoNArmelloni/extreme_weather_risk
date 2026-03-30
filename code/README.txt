Workflow

## Data formatting

The file "questionnaires coded" is the editable dataset used to include interviews results. All the edits including interpretation and revisions are done here. Unfortunately, this is prone to loose track of the changes, but we need to be flexible.

The script coding_treatments.R format the file "questionnaires coded" into tidier versions ( coding_report.csv; coding_report_unc.csv- the second expand uncertainty to the appropriate answers)


## BN creation

The script CPT_HumanSystem.R takes the DAG from the appropriate .net file, parameters from coding_report_unc.csv and build the CPT for the human subsystem

The script CPT_Operation_v2.R takes the DAG from the appropriate .net file, parameters from coding_report_unc.csv and build the CPT for the user subsystem


