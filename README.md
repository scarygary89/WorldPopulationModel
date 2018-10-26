---
title: 'Technical Documentation of World Population Model'
author: "Gary Lin"
bibliography: PopSusBib2.bib
---

In this document, we present a multi-component model and the approach to the integration of the model.  This large multi-component model will be presented in its entirety and proposed methods of integration.  In the previous chapter, we examined how to handle complexity.  Once complexity of the system is characterized quantitatively, we can proceed to model these behaviors as endogenous processes.  We explore ways to build an integrated model that has population dynamics as an endogenous variable in the model that is coupled within a feedback loop.

The intended purpose of this integrated model was four important questions: (i) how does human population growth impact climate change; (ii) what are the feedback loop effects between climate change and economic and social systems; (iii) what is the carrying capacity in terms of food, land, and natural resources and how does the population dynamics in different income regions behave due to an ecological overshoot; and (iv) how will inequality be exacerbated due to these changing factors and what kind of implications does that have on public health (e.g. disparity of food access during to famines).

This chapter aims at exploring how a large integrated model could be developed so calibration and testing are manageable.  We present the framework of the integrated model with the relevant equations that correspond with the mechanics of each submodel, develop a plan of integrations of all model components, and propose and demonstrate an approach to calibrating each submodel.  

# Background
Understanding population growth will be more important than ever.  It has been estimated that 9.7 billion people in 2050 and 11.2 billion people in 2100 by the United Nations [@desa2013world].  The Intergovernmental Panel on Climate Change (IPCC) business-as-usual projections predicting that global mean temperature will rise more than three degrees Celsius by the end of the century which will exacerbate issues such as drought and famines [@mack2005ipcc].  These issues are important and challenging to model since there are complex mechanisms at play.

In general, Bayesian population projection models \cite{gerland2014world,raftery2012bayesian} developed by demographers and statistician have a strong validation but do not account for feedbacks.  Other simpler models without feedback loops were developed as well to provide policymaker with an easy mathematical identity to provide policy-makers with a simple tool that is intuitive, such as the IPAT equation \cite{dietz1994rethinking}. 

In 1971, the Club of Rome at the Massachusetts Institute of Technology commissioned the World 3 model in a publication called \textit{Limits to Growth} \cite{meadows1972limits}.  This model was the first attempt at finding out the anthropogenic impacts and carrying capacity of the world using an integrated model that endogenized the population growth rate.  However, the World 3 model was criticized as having parameters that were not based on existing social theories at the time.  Furthermore, World 3 was accused of sensationalizing the existential threat of anthropogenic effects \cite{cole1973models} (please refer to \cite{bardi2011criticism} for discussion).  Other world models were developed to compete with World 3 \cite{mesarovic1972goal,herrera1976catastrophe,linneman1979moira,roberts1977sarum,onishi1977report,leontief1977future}.  

In 1992, Integrated Assessment Models (IAM) were formally introduced by William Nordhaus, an ardent critic of World 3, developed the Dynamic Integrated Climate Change Model (DICE) \cite{nordhaus1994managing} which eventually won the Nobel Memorial Prize in Economic Sciences in 2018.  DICE introduced a climate damage function and endogenized economic growth and climate change in a closed feedback loop.  However, DICE does not endogenize population growth in their model.  Other IAMs include \cite{peck1992ceta,manne1995merge,manne1992global} and also refer to a review of IAMs \cite{dowlatabadi1995integrated,kelly1999integrated}

Other models that were more theoretical explored growing inequality \cite{motesharrei2014human}.  Models have also explored ways to integrate different submodels \cite{rowan2011integrated,sterman2012climate} within the main feedback loops.  

# Setup

# References