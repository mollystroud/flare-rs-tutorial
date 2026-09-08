# flare-rs-tutorial

Repo to test running FLARE user tool for CEF 🛰️

### OVERVIEW

This repository contains code to run the Forecasting Lake And Reservoir Ecosystems (FLARE) user tool, in which a user may input a lake and time range of interest and create a temperature forecast for that lake.

This code works best when run in a container. Below are the instructions for a user to run this code in a Docker container.

1.  If you do not already have Docker installed, download and install Docker [here](https://docs.docker.com/desktop/setup/install/mac-install/).

2.  Once installed, open both Docker and Terminal on your computer and run the following command in your Terminal:

```         
docker run --platform=linux/amd64 -d -p 8787:8787 -e PASSWORD=yourpassword -e ROOT=TRUE --name FLARE_container rqthomas/flare-rocker:4.4 
```

3.  Open your internet browser of choice and navigate to `http://localhost:8787/`.

4.  Log in with the username `rstudio` and password `yourpassword`. After doing this, the Docker may take a few minutes to start.

5.  Open this GitHub repository in the Docker. To do this, go to File -\> New Project, and select Version Control -\> Git. Then, paste in the GitHub repository url (<https://github.com/mollystroud/flare-rs-tutorial.git>) and click 'Create Project'. This may take a few minutes to load.

6.  Navigate to the terminal in the Docker RStudio session. Type in the following commands:

```         
sudo apt-get update
sudo apt-get install -y python3 python3-pip python3-venv
```

7.  Now, you are ready to begin setting up the tool! Open Tool_Setup.qmd and follow the instructions.
