# copy this file to the <project-root>/devtools-settings.sh and adjust attributes
deps="DeclarativeGraphics DeclarativeGraphics-Cairo" # dependencies seperated by spaces
repo="http://github.com/DeclarativeGraphics"         # root url to download the dependencies

# location where the dependencies' sources/sandboxes are searched
# depslocation="deps"  # using a subfolder (when you only work in this library)
depslocation=".."    # folder containing this project (when you work on the dependent libraries in parallel)
