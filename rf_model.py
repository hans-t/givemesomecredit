import pandas as pd
# from sklearn.ensemble import RandomForestClassifier


def transform(data):
    monthly_income = data['MonthlyIncome']

    na_income = monthly_income.isnull()
    data.loc[na_income, 'MonthlyIncome'] = 1

    zero_income = monthly_income == 0
    data.loc[zero_income, 'MonthlyIncome'] = 1

    data['MonthlyDebtPayment'] = data['DebtRatio'] * monthly_income

    data.loc[na_income, 'MonthlyIncome'] = -1
    data.loc[zero_income, 'MonthlyIncome'] = 0

    return data


training_file = 'data/cs-training.csv'
training = pd.read_csv(
    training_file,
    index_col=0,
)
training = transform(training)

