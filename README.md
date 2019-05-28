# IoT - Analytics
## Deep Analytics, Visualitzation and Modelling
### Task 3: Evaluate Techniques for Wifi Locationing

In the following I will use the machine learning technique to investigate the problem of indoor positioning (with the WAP signal) by using this Data Set: [UJIIndoorLoc Data Set ](http://archive.ics.uci.edu/ml/datasets/UJIIndoorLoc)

### Data Attribute Information:

- **Attribute 001 - 520 (WAP001 - WAP520):** Intensity value for WAP001 - WAP520. Negative integer values from -104 to 0 and +100. Positive value 100 used if WAP001 - WAP520 was not detected.
- **Attribute 521 (Longitude):** Longitude. Negative real values from -7695.9387549299299000 to -7299.786516730871000
- **Attribute 522 (Latitude):** Latitude. Positive real values from 4864745.7450159714 to 4865017.3646842018.
- **Attribute 523 (Floor):** Altitude in floors inside the building. Integer values from 0 to 4.
- **Attribute 524 (BuildingID):** ID to identify the building. Measures were taken in three different buildings. Categorical integer values from 0 to 2.
- **Attribute 525 (SpaceID):** Internal ID number to identify the Space (office, corridor, classroom) where the capture was taken. Categorical integer values.
- **Attribute 526 (RelativePosition):** Relative position with respect to the Space (1 - Inside, 2 - Outside in Front of the door). Categorical integer values.
- **Attribute 527 (UserID):** User identifier (see below). Categorical integer values.
- **Attribute 528 (PhoneID):** Android device identifier (see below). Categorical integer values.
- **Attribute 529 (Timestamp):** UNIX Time when the capture was taken. Integer value.

### Data Set Information:

Many real world applications need to know the localization of a user in the world to provide their services. Therefore, automatic user localization has been a hot research topic in the last years. Automatic user localization consists of estimating the position of the user (latitude, longitude and altitude) by using an electronic device, usually a mobile phone. Outdoor localization problem can be solved very accurately thanks to the inclusion of GPS sensors into the mobile devices. However, indoor localization is still an open problem mainly due to the loss of GPS signal in indoor environments. Although, there are some indoor positioning technologies and methodologies, this database is focused on WLAN fingerprint-based ones (also know as WiFi Fingerprinting).

Although there are many papers in the literature trying to solve the indoor localization problem using a WLAN fingerprint-based method, there still exists one important drawback in this field which is the lack of a common database for comparison purposes. So, UJIIndoorLoc database is presented to overcome this gap. We expect that the proposed database will become the reference database to compare different indoor localization methodologies based on WiFi fingerprinting.

The UJIIndoorLoc database covers three buildings of Universitat Jaume I with 4 or more floors and almost 110.000m2. It can be used for classification, e.g. actual building and floor identification, or regression, e.g. actual longitude and latitude estimation. It was created in 2013 by means of more than 20 different users and 25 Android devices. The database consists of 19937 training/reference records (trainingData.csv file) and 1111 validation/test records (validationData.csv file).

The 529 attributes contain the WiFi fingerprint, the coordinates where it was taken, and other useful information.

Each WiFi fingerprint can be characterized by the detected Wireless Access Points (WAPs) and the corresponding Received Signal Strength Intensity (RSSI). The intensity values are represented as negative integer values ranging -104dBm (extremely poor signal) to 0dbM. The positive value 100 is used to denote when a WAP was not detected. During the database creation, 520 different WAPs were detected. Thus, the WiFi fingerprint is composed by 520 intensity values.

Then the coordinates (latitude, longitude, floor) and Building ID are provided as the attributes to be predicted.

Additional information has been provided.

The particular space (offices, labs, etc.) and the relative position (inside/outside the space) where the capture was taken have been recorded. Outside means that the capture was taken in front of the door of the space.

Information about who (user), how (android device & version) and when (timestamp) WiFi capture was taken is also recorded.
