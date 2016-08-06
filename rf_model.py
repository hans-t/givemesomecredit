from __future__ import print_function
from pprint import pprint

import pandas as pd
from sklearn.metrics import roc_auc_score
from sklearn.grid_search import GridSearchCV
from sklearn.ensemble import RandomForestClassifier
from sklearn.cross_validation import StratifiedKFold


def transform(data):
    monthly_income = data['MonthlyIncome']

    na_income = monthly_income.isnull()
    data.loc[na_income, 'MonthlyIncome'] = 1

    zero_income = monthly_income == 0
    data.loc[zero_income, 'MonthlyIncome'] = 1

    data['MonthlyDebtPayment'] = data['DebtRatio'] * monthly_income

    data.loc[na_income, 'MonthlyIncome'] = -1
    data.loc[zero_income, 'MonthlyIncome'] = 0

    data['DebtIncomeDiff'] = data['MonthlyDebtPayment'] - monthly_income

    data.loc[data['NumberOfDependents'].isnull(), 'NumberOfDependents'] = -1

    data['TotalDaysLate'] = data['NumberOfTime30-59DaysPastDueNotWorse'] + \
                                data['NumberOfTime60-89DaysPastDueNotWorse'] + \
                                data['NumberOfTimes90DaysLate']

    return data


def get_labels_and_columns(df):
    labels = df['SeriousDlqin2yrs'].values
    columns = df.ix[:, df.columns != 'SeriousDlqin2yrs'].values
    return labels, columns


def read_data(filename):
    df = pd.read_csv(filename, index_col=0)
    df = transform(df)
    return get_labels_and_columns(df)


def fit_classifier(labels, columns):
    param_grid = {
        'n_estimators': [1300, 1500, 1700, 1900],
        'max_features': [4],
        'min_samples_leaf': [140],
        'bootstrap': [True],
        'class_weight': ['balanced_subsample'],
    }
    cv_generator = StratifiedKFold(y=labels, n_folds=5, shuffle=True)
    classifier = RandomForestClassifier(random_state=10, verbose=1)
    grid_search = GridSearchCV(
        classifier,
        param_grid=param_grid,
        scoring='roc_auc',
        n_jobs=-1,
        cv=cv_generator,
        refit=True,
        verbose=1,
    )
    grid_search.fit(X=columns, y=labels)
    print('\n' + '*'*78)
    print("Grid scores:")
    pprint(grid_search.grid_scores_)
    print("Best AUC:", grid_search.best_score_)
    print("Best parameters:", grid_search.best_params_)
    print('*'*78 + '\n')
    return grid_search


def score(classifier, columns):
    return classifier.predict_proba(X=columns)[:,1]


def save_score(filename, score):
    df = pd.DataFrame(score, columns=['Probability'])
    df.index += 1
    df.to_csv(filename, index_label='Id')


if __name__ == '__main__':
    train_filename = 'data/cs-training.csv'
    train_labels, train_columns = read_data(train_filename)
    classifier = fit_classifier(train_labels, train_columns)

    print('Reading test set.')
    test_filename = 'data/cs-test.csv'
    _, test_columns = read_data(test_filename)

    print('Scoring test set.')
    scored_test_set = score(classifier, columns=test_columns)

    print('Saving scored test set.')
    save_score(filename='rf-prediction.csv', score=scored_test_set)
