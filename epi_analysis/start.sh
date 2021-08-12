cd /utilities && python3 autoload.py
R -e "require(shiny); runApp('/shinyapp', host='0.0.0.0', port=3838)"
