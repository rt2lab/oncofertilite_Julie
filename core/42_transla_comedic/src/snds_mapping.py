import pandas as pd
import numpy as np
import urllib.request
import xml.etree.ElementTree as ET

def preprocess_df(df):
    """

    :param df: initial dataframe of top_150_snds
    :return new_df: preprocessed dataframe without drug combinations
    """
    names = list(df['level5_lib'])
    atcs = df['code_atc5']
    new_df = df.copy()
    new_df = new_df.set_index('code_atc5')
    for i in range(len(df)):
        name = names[i]
        atc = atcs[i]

        patt = ' and '
        if patt  in name:
            drugs = name.split(patt)
            for drug in drugs:
                drug = drug.capitalize()
                drug = drug.strip()
                if drug not in names:
                    drug_dict = {'level5_lib':drug,'count':np.nan,
                                'in_top_150':np.nan}
                    drug_df = pd.DataFrame(drug_dict,index={np.nan:'code_atc5'})
                    new_df = pd.concat([new_df,drug_df])
            new_df= new_df.drop(labels=atc,axis=0)
    new_df = new_df.reset_index()
    new_df = new_df.rename(columns= {'index':'code_atc5'})
    new_df = new_df.drop_duplicates()
    return new_df

def create_infotab(df):
    """

    :param df: inital top150 before cleaning
    :return problem_df: all the drugs that need to be preprocessed
    """
    to_delete = ['Avocado and soyabean oil, unsaponifiables','Ginkgo folium','Potassium clorazepate',
                 'Ferrous sulfate','Calcium carbonate']
    atcs = []
    counts = []
    tops = []
    problematic_drugs = []
    decisions = []
    for i in range(len(df)):
        name = df['level5_lib'].iloc[i]
        atc = df['code_atc5'].iloc[i]
        count = df['count'].iloc[i]
        top = df['in_top_150'].iloc[i]
        if name in to_delete:
            problematic_drugs.append(name)
            atcs.append(atc)
            counts.append(count)
            tops.append(top)
            decisions.append('to delete ')
        if ('combinations' in name):
            problematic_drugs.append(name)
            atcs.append(atc)
            counts.append(count)
            tops.append(top)
            decisions.append(' ')
        elif  (' and ' in name):
            problematic_drugs.append(name)
            atcs.append(atc)
            counts.append(count)
            tops.append(top)
            decisions.append('break in two drugs')
    problems_df = pd.DataFrame({'code_atc5':atcs,'level5_lib':problematic_drugs,
                               'count':counts,'in_top_150':tops,'decision':decisions})
    return problems_df

def atc_to_pubchemID(df):
    """

    :param df: dataframe with of atc drugs with at least two columns; code_atc5 for the drugs atc codes, level5_lib for
               the drug names
    :return new_df: dataframe with a new column pubchem_id containing the pubchem substance id of each drug
            errors_df: dataframe with the atc codes and names of the drugs without pubchem correspondences
    """
    atc_codes = list(df['code_atc5'])
    names = list(df['level5_lib'])
    pubchem_id = []
    for i in range(len(df)):
        atc = atc_codes[i]
        name = names[i]
        print(atc)
        try:
            link = 'https://pubchem.ncbi.nlm.nih.gov/rest/pug/substance/name/' + atc + '/synonyms/XML'
            with urllib.request.urlopen(link) as f:
                tree = ET.parse(f)
                root = tree.getroot()
                pubchem_id.append(root[0][0].text)
        except :
            print('*** Atc without pubchem correspondence',atc)
            print('Drug naame:',name)
            print('Check for name correspondences for',name)
            link = 'https://pubchem.ncbi.nlm.nih.gov/rest/pug/substance/name/'+name+'/synonyms/XML'
            print(link)
            try:
                with urllib.request.urlopen(link) as f:
                    tree = ET.parse(f)
                    root = tree.getroot()
                    pubchem_id.append(root[0][0].text) 
            except :
                print('There is no correspondence')
                pubchem_id.append(np.nan)

    new_df = df.copy()
    new_df['pubchem_id'] = pubchem_id
    #errors_df = pd.DataFrame({'code_atc5':errors_atc,'level5_lib':errors_name})
    return new_df