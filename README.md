# Green predict

Green predict app is part of my bachelor thesis. It can predict electricity production of photovoltaic power plants from weather forecast.
Application works for now just with static historic data.

#### Code is divided into 3 parts(modules):
- **Data import** is responsible importing data sets and enriching them with additional parameters
- **Prediction model** is based on neural networks and is responsible for training, testing and comparing different configurations of neural networks.
- **Web application** is implemented with Shiny framework is responsible for data visualization and model predictions presentation to the end user.

Web site of the application: [https://simonsudora.shinyapps.io/green_predict_app/](https://simonsudora.shinyapps.io/green_predict_app/).

[Presentation of my bechalor thesis](http://www.slideshare.net/SimonSudora/prediction-of-electricity-generation-from-photovoltaic) (in Slovak) focused on model results and accuracy.

You can find [the full text of my bachelor thesis](http://opac.crzp.sk/?fn=detailBiblioForm&sid=B0ECE52FE577B8B951C262E96922) 
(in Slovak) in [CRZP](http://cms.crzp.sk/).


## Screenshots:
![data dipendencies visualization](http://i.imgur.com/6zFjkVk.png "data dipendencies visualization")

![model predictions a their visalization](http://i.imgur.com/lcFvC33.png "model predictions a their visalizations")

*Note: I didn't use github versioning from the begging of the project.*
