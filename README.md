Deprecated. Up to date version can be found at https://github.com/openconnectome/Vector-Explorer.

# NeuroData's Vector Explorer
Web-based vector analytics and some visualization

To run:

1. go to the shiny dir
2. start R
3. type `library('shiny')`
5. type `runApp()`

# Deploy Updates

from terminal:

1. ```ssh brainapps1.neurodata.io``` and enter your pword
2. ```cd /srv/shinyapps/Vector-Explorer```
3.  ```git pull```
4.   (maybe restart shiny)

# If docker container goes down

(below are instructions for graph explorer, not vector explorer)

1. ```./graphexplorerstart.sh```, which lives in ```/srv/shinyapps/Graph-Explorer```

# If we want to add more dependencies to installation

- Update dockerfile
- Run ```sudo docker build -t graph_explorer .``` in the same directory as the Dockerfile 

# To get a terminal on the docker container

- ```docker exec -it <<container name>> /bin/bash```
- Find the container name by running ```docker ps```
