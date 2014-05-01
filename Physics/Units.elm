module Physics.Units where

--UNITS

--Time
-- SI
milliseconds s = s
seconds = ((*) 1000) . milliseconds

-- Non-SI
minutes   = ((*) 60  ) . seconds
hours     = ((*) 60  ) . minutes
days      = ((*) 24  ) . hours
years     = ((*) 365 ) . days
decades   = ((*) 10  ) . years
centuries = ((*) 100 ) . years
millenia  = ((*) 1000) . years


--Distance
-- SI
meters m = m
millimeters = ((*) 0.001) . meters
centimeters = ((*) 0.01 ) . meters
decimeters  = ((*) 0.1  ) . meters
decameters  = ((*) 10   ) . meters
hectometers = ((*) 100  ) . meters
kilometers  = ((*) 1000 ) . meters

-- Non-SI
inches        = ((*) 25.4 ) . millimeters
feet          = ((*) 12   ) . inches
yards         = ((*) 3    ) . feet
miles         = ((*) 1760 ) . yards
leagues       = ((*) 3    ) . miles
fathoms       = ((*) 2    ) . yards
cables        = ((*) 120  ) . fathoms
nauticalMiles = ((*) 8.439) . cables

--Mass
-- SI
kilograms kg = kg
grams      = ((*) 0.001) . kilograms
milligrams = ((*) 0.001) . grams
centigrams = ((*) 0.01 ) . grams
decigrams  = ((*) 0.1  ) . grams
decagrams  = ((*) 10   ) . grams
hectograms = ((*) 100  ) . grams

-- Non-SI
tons      = ((*) 1000    ) . kilograms
tonnes    = tons
grains    = ((*) 64.79891) . milligrams
drams     = ((*) 1.771845) . grams
ounces    = ((*) 28.34952) . grams
pounds    = ((*) 0.453592) . kilograms
shortTons = ((*) 907.1847) . kilograms
longTons  = ((*) 1016.047) . kilograms
carats    = ((*) 200     ) . milligrams