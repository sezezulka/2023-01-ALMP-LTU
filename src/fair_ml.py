"""
Functions for analysing fairness of prediction.
"""

import numpy as np
import pandas as pd
from sklearn import metrics
import matplotlib.pyplot as plt


def pred_threshold_risk(df, pred_prob_name, policy_name, percentile1, percentile2=None):
    """
    Prediction based on threshold determined by percentile of risk score.

    Parameters:
    ---------
    df : pd.DataFrame
        DataFrame includes predicted risk scores.
    pred_prob_name : str
        name of variable containing predicted risk score
    policy_name : str
        name of policy
    precentile1 : int
        threshold as precentile of risk score; if two given then lower bound of interval
    precentile2 : int
        if given, upper bound of interval of thresholds


    Returns:
    ---------
    preds : pd.DataFrame
        DataFrame containing the predictions
    """
    pred_probs = df[pred_prob_name]

    if percentile2 is None:
        c = np.percentile(pred_probs, percentile1)
        preds = pd.DataFrame(np.where(pred_probs >= c, 1, 0))
        preds.columns = ['y_hat_{}'.format(policy_name)]
    else:
        c1, c2 = np.percentile(pred_probs, [percentile1, percentile2])
        preds = pd.DataFrame(np.where(((pred_probs >= c1) & (pred_probs <= c2)), 1, 0))
        preds.columns = ['y_hat_{}'.format(policy_name)]
        c = [c1, c2]

    return preds, c


def fair_dict(df_pred, y_names, sens_attributes):
    """
    Makes dictionary with subsetted predictions by sensitive attributes.

    Parameters:
    ---------
    df_pred : pd.DataFrame
        contains sensitive attributes and predictions
    y_names : list
        List of predictions
    sens_attributes : dictionary
        dict of binary sensitive attributes: {Name : var_name}

    Returns:
    ---------
    dict_fairness : dictonary

    """
    dict_fairnes = {}

    for i in sens_attributes:
        dict_fairnes[i] = {}

        dict_fairnes[i]['Overall'] = df_pred[y_names]
        dict_fairnes[i]['1'] = df_pred[df_pred[sens_attributes[i]] == 1][y_names]
        dict_fairnes[i]['0'] = df_pred[df_pred[sens_attributes[i]] == 0][y_names]

    return dict_fairnes


def fair_confusion_matrix_plot(sens_dict, sens_name, y_name, pred_name, normalize):
    """
    Plot three confusion matrices: overall and for a binary senstive attribute
    together with classification report.

    Parameters:
    ---------
    sens_dict : dictonary
        sensitive attribute, sliced predictions respectivley
    sens_name : str
        name of senstive attribute
    y_name : str
        name of target variable
    pred_name : str
        name of prediction rule
    normalize : str
        type of confusion matrix

    Returns:
    ---------

    """

    f, axes = plt.subplots(1, 3, figsize=(20, 5), sharey='none')

    for i, (key, df) in enumerate(sens_dict[sens_name].items()):

        print('Classification report: ' + sens_name + ', ' + key + ', ' + pred_name)
        print(metrics.classification_report(df[y_name], df[pred_name], output_dict=True))

        cm = metrics.confusion_matrix(df[y_name], df[pred_name], normalize=normalize)
        disp = metrics.ConfusionMatrixDisplay(cm)
        disp.plot(ax=axes[i])
        disp.ax_.set_title(key)
        disp.im_.colorbar.remove()
        disp.ax_.set_xlabel('')
        if i != 0:
            disp.ax_.set_ylabel('')

    f.text(0.4, 0.1, 'Predicted label', ha='left')
    plt.subplots_adjust(wspace=0.40, hspace=0.1)

    f.colorbar(disp.im_, ax=axes)
    f.suptitle('Confusion matrix: ' + sens_name + ', ' + pred_name)


def fair_risk_score_plot(sens_dict, sens_name, pred_name, thresholds):
    """
    Plot risk score of classifier overall and by binary senstive attributes.

    Parameters:
    ---------
    sens_dict : dictonary
        sensitive attribute, sliced predictions respectivley
    sens_name : str
        name of senstive attribute
    pred_name : str
        name of prediction rule
    thresholds : list
        list of policy thresholds

    Returns:
    ---------
    """

    for (key, df) in sens_dict[sens_name].items():
        plt.hist(df[pred_name], bins=100, alpha=0.5, label=key)
    plt.title('Histogram Risk Score (predicted probability), ' + sens_name + ', ' + pred_name)
    plt.legend()
    for i in thresholds:
        plt.axvline(i)


def fair_roc_curve_plot(sens_dict, sens_name, y_name, pred_name):
    """
    Plot ROC curve of overall classifier and by binary senstive attribute

    Parameters:
    ---------
    sens_dict : dictonary
        sensitive attribute, sliced predictions respectivley
    sens_name : str
        name of senstive attribute
    y_name : str
        name of target variable
    pred_name : str
        name of prediction rule

    Returns:
    ---------
    """
    plt.figure()
    lw = 2

    for i, (key, df) in enumerate(sens_dict[sens_name].items()):
        fpr, tpr, thresholds = metrics.roc_curve(df[y_name], df[pred_name], pos_label=1)
        roc_auc = metrics.auc(fpr, tpr)

        plt.plot(
            fpr,
            tpr,
            color='C' + str(i),
            alpha = 0.7,
            lw=lw,
            label="ROC curve, " + str(key) + " (area = %0.2f)" % roc_auc,
        )
    plt.plot([0, 1], [0, 1], color="navy", lw=lw, linestyle="--")
    plt.xlim([0.0, 1.0])
    plt.ylim([0.0, 1.05])
    plt.xlabel("False Positive Rate")
    plt.ylabel("True Positive Rate")
    plt.title("ROC-Curve, " + sens_name + ', ' + pred_name)
    plt.legend(loc="lower right")


def fair_independence_test(sens_dict, sens_attributes, pred_name, slack):
    """
    """
    p1 = []
    p0 = []

    for i in sens_attributes:
        p_1 = sens_dict[i]['1'][pred_name].mean()
        p_0 = sens_dict[i]['0'][pred_name].mean()

        print('P(y_hat=1|A=1)= ' + str(np.round(p_1, 3)))
        print('P(y_hat=1|A=0)= ' + str(np.round(p_0, 3)))

        if np.abs(p_0 - p_1) <= slack:
            print('{}: Independence meet for {} at slack: {}.'.format(pred_name, i, slack))
        else:
            print('{}: Independence not(!) meet for {} at slack: {}'.format(pred_name, i, slack))

        p1.append(p_1)
        p0.append(p_0)

    print(p1)
    print(p0)

    return np.subtract(np.array(p0), np.array(p1))


def fair_separation_test(sens_dict, sens_attributes, y_name, pred_name, slack):
    """
    Equalized odds, Separation, Positive Rate Parity
    Y_hat={0,1}|A={0, 1}
    for binary predicition and sensitive feature, A=1 sensitive feature
    """
    p1_tpr = []
    p0_tpr = []
    p1_fpr = []
    p0_fpr = []

    for i in sens_attributes:
        p_1_tpr = sens_dict[i]['1'][sens_dict[i]['1'][y_name] == 1][pred_name].mean()
        p_0_tpr = sens_dict[i]['0'][sens_dict[i]['0'][y_name] == 1][pred_name].mean()

        p_1_fpr = sens_dict[i]['1'][sens_dict[i]['1'][y_name] == 0][pred_name].mean()
        p_0_fpr = sens_dict[i]['0'][sens_dict[i]['0'][y_name] == 0][pred_name].mean()

        print('P(y_hat=1|A=1,Y=1)= ' + str(np.round(p_1_tpr, 3)))
        print('P(y_hat=1|A=0,Y=1)= ' + str(np.round(p_0_tpr, 3)))
        print('P(y_hat=1|A=1,Y=0)= ' + str(np.round(p_1_fpr, 3)))
        print('P(y_hat=1|A=0,Y=0)= ' + str(np.round(p_0_fpr, 3)))

        if (np.abs(p_0_tpr - p_1_tpr) <= slack) & (np.abs(p_0_fpr - p_1_fpr) <= slack):
            print('{}: Separation meet for {} at slack: {}'.format(pred_name, i, slack))
        else:
            print('{}: Separation not(!) meet for {} at slack: {}'.format(pred_name, i, slack))

        p1_tpr.append(p_1_tpr)
        p0_tpr.append(p_0_tpr)
        p1_fpr.append(p_1_fpr)
        p0_fpr.append(p_0_fpr)

    #print(p1_tpr)
    #print(p0_tpr)
    return np.subtract(np.array(p0_tpr), np.array(p1_tpr)), np.subtract(np.array(p0_fpr), np.array(p1_fpr))


def fair_sufficiency_test(sens_dict, sens_attributes, y_name, pred_name, slack):
    """
    Predictive Rate Parity, Sufficiency
    Y_hat={0,1}|A={0, 1}
    for binary predicition and sensitive feature, A=1 sensitive feature

    P₀ [Y = y| C= c] = P₁ [Y = y| C= c] ∀ y, c ∈ {0,1}

    This is equivalent to satisfying both
    P₀ [Y = 1| C= 1] = P₁ [Y = 1| C= 1] and
    P₀ [Y = 0| C= 0] = P₁ [Y = 0| C= 0]
    """
    p00 = []
    p01 = []
    p10 = []
    p11 = []

    for i in sens_attributes:
        p_10 = sens_dict[i]['1'][sens_dict[i]['1'][pred_name] == 0][y_name].mean()
        p_00 = sens_dict[i]['0'][sens_dict[i]['0'][pred_name] == 0][y_name].mean()

        p_11 = sens_dict[i]['1'][sens_dict[i]['1'][pred_name] == 1][y_name].mean()
        p_01 = sens_dict[i]['0'][sens_dict[i]['0'][pred_name] == 1][y_name].mean()

        print('P(y=1|A=1,y_hat=0)= ' + str(np.round(p_10, 3)))
        print('P(y=1|A=0,y_hat=0)= ' + str(np.round(p_00, 3)))
        print('P(y=1|A=1,y_hat=1)= ' + str(np.round(p_11, 3)))
        print('P(y=1|A=0,y_hat=1)= ' + str(np.round(p_01, 3)))

        if (np.abs(p_00 - p_10) <= slack) and (np.abs(p_01 - p_11) <= slack):
            print('{}: Sufficency meet for {} at slack: {}'.format(pred_name, i, slack))
        else:
            print('{}: Sufficency not(!) meet for {} at slack: {}'.format(pred_name, i, slack))

        p00.append(p_00)
        p01.append(p_01)
        p10.append(p_10)
        p11.append(p_11)

    return np.subtract(np.array(p00), np.array(p10)), np.subtract(np.array(p01), np.array(p11))