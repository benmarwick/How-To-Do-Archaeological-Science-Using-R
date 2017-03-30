FROM rocker/rstudio-stable:3.3.3
## Work-around to make Docker Hub use the Dockerfile from https://github.com/rocker-org/rocker-versioned/rstudio

# Install Java, https://github.com/dockerfile/java/blob/master/openjdk-7-jdk/Dockerfile, this is the one that similitude/netlogo-docker is FROM
RUN \
  apt-get update && \
  apt-get install -y openjdk-7-jdk && \
  rm -rf /var/lib/apt/lists/*

# Define working directory.
WORKDIR /data

# Define commonly used JAVA_HOME variable
ENV JAVA_HOME /usr/lib/jvm/java-7-openjdk-amd64

# Install Netlogo
# From https://github.com/similitude/netlogo-docker/blob/master/Dockerfile
ENV NETLOGO_HOME /opt/netlogo

# Download and extract NetLogo to /opt/netlogo.
RUN wget https://ccl.northwestern.edu/netlogo/5.1.0/netlogo-5.1.0.tar.gz && \
    tar xzf netlogo-5.1.0.tar.gz && \
    rm netlogo-5.1.0.tar.gz && \
    mv netlogo-5.1.0 $NETLOGO_HOME

# usage details: https://github.com/NetLogo/NetLogo/wiki/Controlling-API
