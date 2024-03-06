


### Clone dockerfile from githuib
> git clone git@github.com:nsekhon1991/pysplit.git
> cd pysplit

### Build docker image
> docker build -t pysplit . 
# pysplitv2 is with Ubunntu build

### Run your image and enter into it
> docker run -it pysplit /bin/bash

### Remove containner
> docker rm pysplit

### Download Example data on local comp and mount to docker container
### AND MOUNTING HYPSLIT and data folder
>
  docker run --name pysplit \
           -v /Volumes/'Extreme SSD'/'Hysplit_Data'/'Example':/pysplit/docs/data \
           -v /Users/natashasekhon/Documents/Projects/git_repo/pysplit/hysplit.v5:/pysplit/docs/Hysplitv5 \
           -v /Users/natashasekhon/Documents/Projects/git_repo/pysplit/results:/pysplit/docs/temp_store/results \
           -it pysplit /bin/bash

### UPDATE SEPT29, downloaded data using filezilla on external hardrive
### new path for example data
> /Volumes/'Extreme SSD'/'Hysplit_Data'/'Example'

### Run PySplit

> cd /pysplit/docs/examples
> python3 bulk_trajgen_example.py

### Save plots 
Make following additions to examples/basic_plotting_example.py: 
> Add matplotlib to file: import matplotlib.pyplot as plt
> Edit traj path: trajgroup : pysplit.make_trajectorygroup('/pysplit/docs/temp_store/*jan*')
> Safe plot at the end : plt.savefig('/pysplit/docs/temp_store/results/result.png')


### TO DOWNLOAD EXAMPLE DATA
# open filezilla
# in URL paste ftp.arl.noaa.gov
# type in username and password a
# go to folder archives/gdas01

### Moved ASCDATA.CFG  to the same folder with CONTROL file, the temp_work
### FOR TESTING, manullay copied file into container using: 
docker cp hysplit.v5/testing/ASCDATA.CFG pysplit:/pysplit/docs/temp_work/

### mv Test.eps to external harddrive for viewing 
