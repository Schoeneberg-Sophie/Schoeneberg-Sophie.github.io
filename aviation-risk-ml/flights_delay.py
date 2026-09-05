import pandas as pd

from sklearn.pipeline import Pipeline
from sklearn.preprocessing import StandardScaler
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import (
    accuracy_score,
    precision_score,
    recall_score,
    f1_score,
    confusion_matrix,
)


january = pd.read_csv("january_flights_with_weather.csv")
february = pd.read_csv("february_flights_with_weather.csv")


features = ["ORIGIN_AWND_lag1","ORIGIN_AWND_lag2","ORIGIN_PRCP_lag1","ORIGIN_PRCP_lag2","ORIGIN_TMIN_lag1","ORIGIN_TMIN_lag2","ORIGIN_TMAX_lag1",
    "ORIGIN_TMAX_lag2","ORIGIN_SNOW_lag1","ORIGIN_SNOW_lag2","DEST_AWND_lag1","DEST_AWND_lag2","DEST_PRCP_lag1","DEST_PRCP_lag2","DEST_TMIN_lag1","DEST_TMIN_lag2",
    "DEST_TMAX_lag1","DEST_TMAX_lag2","DEST_SNOW_lag1","DEST_SNOW_lag2",
]

# A flight is considered delayed if arrival delay >= 15 minutes
for df in [january, february]:
    df["IS_DELAYED"] = (
        pd.to_numeric(df["ARR_DELAY"], errors="coerce") >= 15
    ).astype(int)

    df.dropna(
        subset=features + ["IS_DELAYED"],
        inplace=True
    )

# January is used to train the model
X_train = january[features]
y_train = january["IS_DELAYED"]


# February is kept separate for out-of-sample testing
X_test = february[features]
y_test = february["IS_DELAYED"]

# Standardise the weather variables and train a logistic regression model (for flight delayed yes/no (0/1))
model = Pipeline([
    ("scaler", StandardScaler()),
    ("classifier", LogisticRegression(
        class_weight="balanced",
        random_state=42,
        max_iter=1000
    ))
])

# train the model on January and predict delays in February
model.fit(X_train, y_train)
y_pred = model.predict(X_test)

# Compare the predictions with the actual February delays
accuracy = accuracy_score(y_test, y_pred)
precision = precision_score(y_test, y_pred)
recall = recall_score(y_test, y_pred)
f1 = f1_score(y_test, y_pred)


print(f"Accuracy:  {accuracy:.2%}")
print(f"Precision: {precision:.2%}")
print(f"Recall:    {recall:.2%}")
print(f"F1 Score:  {f1:.2%}")

print("\nConfusion Matrix:")
print(confusion_matrix(y_test, y_pred))