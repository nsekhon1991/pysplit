FROM ubuntu:20.04

ENV DEBIAN_FRONTEND noninteractive

#FROM  python:3.7-buster

RUN apt-get update  
RUN  apt-get install -y python3 \
                       python3-pip\
                       python-mpltoolkits.basemap \
                       libgeos-c1v5 \
                       libgeos-dev \
                       libgeos++-dev \
                       proj-bin \
                       libproj-dev\
                       vim\
                       libgfortran5\
                       git-all

##RUN echo "" && dpkg -l

##RUN python3.7 --version

RUN pip3 install numpy==1.20.3 \
                 matplotlib==3.4.2 \
                 geopandas==0.9.0 \
                 scipy==1.7.0\
                 pyshp==2.1.3\
                 cartopy==0.19.0.post1\
                 pysplit==0.3.6\
                 shapely==1.8.5
                    

#RUN  apt-get install -y geos

RUN pip3 install basemap
#RUN pip3 install https://github.com/matplotlib/basemap/archive/master.zip
RUN git clone https://github.com/mscross/pysplit.git

RUN mkdir /pysplit/docs/temp_work
RUN mkdir /pysplit/docs/temp_store
RUN mkdir /pysplit/docs/temp_store/results

COPY ./hysplit.v5/testing/ASCDATA.CFG /pysplit/docs/temp_work/


RUN sed -i "s/"r\'C:\\/hysplit4\\/working\'"/\'\/pysplit\/docs\/temp_work\'/g" /pysplit/docs/examples/bulk_trajgen_example.py
RUN sed -i "s/"r\'C:\\/trajectories\\/colgate\'"/\'\/pysplit\/docs\/temp_store\'/g" /pysplit/docs/examples/bulk_trajgen_example.py
RUN sed -i "s/"r\'E:\\/gdas\'"/\'\/pysplit\/docs\/data\'/g" /pysplit/docs/examples/bulk_trajgen_example.py
RUN sed -i "s/get\_clipped\\=True/get\_clipped\\=True,hysplit\\=\'\/pysplit\/docs\/Hysplitv5\/exec\/hyts_std\'/g" /pysplit/docs/examples/bulk_trajgen_example.py

### cd pysplit/pysplit/docs/examples/
### apt-get install ftp
### ftp 
### open ftp.arl.noaa.gov
### close

