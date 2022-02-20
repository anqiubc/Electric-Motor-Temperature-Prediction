# Electric-Motor-Temperature-Prediction
Multiple Linear Regression, Principal Component Regression (PCA Regression).

With R. Initial attempt in June, 2020.

## Dataset
Source: https://www.kaggle.com/wkirgsn/electric-motor-temperature 
The data set comprises several sensor data collected from a permanent magnet synchronous motor (PMSM) deployed on a test bench. The PMSM represents a german OEM's prototype model. Test bench measurements were collected by the LEA department at Paderborn University.

#### Variables:
- u_q: q component of Voltage measured in Volts
- u_d: d component of Voltage measured in Volts
- i_q: q component of Current measured in Amps
- i_d: d component of Current measured in Amps
- ambient: ambient temperature around the stator in °C (measured by a thermal sensor fixed close to stator)
- coolant: motor coolant (water in this case) temperature of the motor in °C (measured by a fixed thermal sensor at coolant outlet)
- motor speed: ambient temperature around the stator in °C (measured by a fixed thermal sensor)
- stator_tooth: stator tooth temperature in °C
- stator_winding: stator winding temperature in °C
- stator_yoke: stator yoke temperature in °C
- pm: permanent magnet tooth temperature in °C
- profile_id: id of the measurement session

## Target
- The most interesting target features are **rotor temperature ("pm"), stator temperatures ("stator_*") and torque**.
Especially rotor temperature and torque are not reliably and economically measurable in a commercial vehicle. Being able to have strong estimators for the rotor temperature helps the automotive industry to manufacture motors with less material and enables control strategies to utilize the motor to its maximum capability.
- Therefore, the target for today is to predict the **rotor temperature**("pm") of a given motor.

## Conclusion
- Built a multiple linear regression model to accurately predict the rotor temperature of a given motor on a data set with over a million entries of 13 variables with a small MSE. Performed a complete model diagnostic and variable selection process. 
- Also applied principal component regression (PCA Regression) considering the collinearity exists in the predictors. 