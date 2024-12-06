# Set up Shinyapps.io account for https
rsconnect::setAccountInfo(name='[Add your own name here]', 
                          token='[Add your own token here]', 
                          secret='[Add your own token here]')

# Deploy the app to Shinyapps.io
rsconnect::deployApp('address/')
