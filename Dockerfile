FROM flangelier/scala

RUN wget https://github.com/fvillalobos14/LP_Scala/archive/master.zip

RUN unzip master.zip

EXPOSE 8000

CMD cd LP_Scala-master && scala hmwrk.scala