#IMPORTATION DES BIBLIOTHÈQUES
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import warnings
warnings.filterwarnings('ignore')

# Machine Learning
from sklearn.model_selection import train_test_split, cross_val_score, GridSearchCV
from sklearn.preprocessing import LabelEncoder, StandardScaler, MinMaxScaler
from sklearn.linear_model import LinearRegression, Ridge, Lasso, ElasticNet
from sklearn.ensemble import RandomForestRegressor, GradientBoostingRegressor, ExtraTreesRegressor
from sklearn.tree import DecisionTreeRegressor
from sklearn.svm import SVR
from sklearn.neighbors import KNeighborsRegressor
from sklearn.metrics import mean_squared_error, r2_score, mean_absolute_error
from sklearn.feature_selection import SelectKBest, f_regression, RFE
from sklearn.decomposition import PCA

# Statistiques
from scipy import stats
from scipy.stats import pearsonr, spearmanr

# Configuration des graphiques
plt.style.use('seaborn-v0_8')
sns.set_palette("husl")
plt.rcParams['figure.figsize'] = (12, 8)

print("="*80)
print("🔋 ANALYSE COMPLÈTE DES DONNÉES ÉNERGÉTIQUES USA")
print("="*80)

# 📂 CHARGEMENT DES DONNÉES
df = pd.read_csv("energy_usa.csv")
print(f"\n📊 Dataset chargé avec succès: {df.shape[0]} lignes, {df.shape[1]} colonnes")


# =============================================================================
# 🔍 1. ANALYSE EXPLORATOIRE APPROFONDIE (EDA)
# =============================================================================

print("\n" + "="*60)
print("🔍 1. ANALYSE EXPLORATOIRE APPROFONDIE")
print("="*60)

# 1.1 Informations générales
print("\n📋 1.1 INFORMATIONS GÉNÉRALES")
print("-" * 40)
print(df.info())
print(f"\nTaille du dataset: {df.shape}")
print(f"Types de données:\n{df.dtypes.value_counts()}")

# 1.2 Statistiques descriptives détaillées
print("\n📊 1.2 STATISTIQUES DESCRIPTIVES")
print("-" * 40)
print(df.describe(include='all'))

# Statistiques pour variables numériques
numeric_cols = df.select_dtypes(include=[np.number]).columns
print(f"\n📈 Variables numériques: {list(numeric_cols)}")

for col in numeric_cols:
    print(f"\n{col}:")
    print(f"  • Médiane: {df[col].median():.2f}")
    print(f"  • Mode: {df[col].mode().iloc[0] if not df[col].mode().empty else 'N/A'}")
    print(f"  • Asymétrie (Skewness): {df[col].skew():.2f}")
    print(f"  • Aplatissement (Kurtosis): {df[col].kurtosis():.2f}")

# 1.3 Détection des valeurs manquantes et aberrantes
print("\n🔍 1.3 QUALITÉ DES DONNÉES")
print("-" * 40)
missing_data = df.isnull().sum()
print("Valeurs manquantes par colonne:")
print(missing_data[missing_data > 0])

if missing_data.sum() == 0:
    print("✅ Aucune valeur manquante détectée!")

# Détection des outliers avec IQR
print("\n🚨 Détection des valeurs aberrantes (méthode IQR):")
for col in numeric_cols:
    Q1 = df[col].quantile(0.25)
    Q3 = df[col].quantile(0.75)
    IQR = Q3 - Q1
    lower_bound = Q1 - 1.5 * IQR
    upper_bound = Q3 + 1.5 * IQR
    outliers = df[(df[col] < lower_bound) | (df[col] > upper_bound)]
    print(f"  • {col}: {len(outliers)} outliers ({len(outliers)/len(df)*100:.1f}%)")

# 1.4 Analyse temporelle
print("\n📅 1.4 ANALYSE TEMPORELLE")
print("-" * 40)
if 'Year' in df.columns:
    print(f"Période couverte: {df['Year'].min()} - {df['Year'].max()}")
    print(f"Nombre d'années: {df['Year'].nunique()}")
    
if 'Month' in df.columns:
    print(f"Mois couverts: {sorted(df['Month'].unique())}")
    monthly_avg = df.groupby('Month')['Total Renewable Energy'].mean()
    print(f"Mois avec le plus d'énergie renouvelable: {monthly_avg.idxmax()} ({monthly_avg.max():.2f})")

# 1.5 Analyse par secteur
print("\n🏭 1.5 ANALYSE PAR SECTEUR")
print("-" * 40)
if 'Sector' in df.columns:
    sector_stats = df.groupby('Sector')['Total Renewable Energy'].agg(['count', 'mean', 'std', 'min', 'max'])
    print(sector_stats)


# =============================================================================
# 📈 2. VISUALISATIONS AVANCÉES
# =============================================================================

print("\n" + "="*60)
print("📈 2. VISUALISATIONS AVANCÉES")
print("="*60)

# 2.1 Distribution des variables
fig, axes = plt.subplots(2, 2, figsize=(15, 12))
fig.suptitle("Distribution des Variables Principales", fontsize=16, fontweight='bold')

# Histogramme de la variable cible
axes[0,0].hist(df['Total Renewable Energy'], bins=30, alpha=0.7, color='skyblue', edgecolor='black')
axes[0,0].set_title('Distribution: Total Renewable Energy')
axes[0,0].set_xlabel('Total Renewable Energy')
axes[0,0].set_ylabel('Fréquence')

# Box plot par secteur
if 'Sector' in df.columns:
    df.boxplot(column='Total Renewable Energy', by='Sector', ax=axes[0,1])
    axes[0,1].set_title('Distribution par Secteur')
    axes[0,1].tick_params(axis='x', rotation=45)

# Évolution temporelle
if 'Year' in df.columns:
    yearly_trend = df.groupby('Year')['Total Renewable Energy'].mean()
    axes[1,0].plot(yearly_trend.index, yearly_trend.values, marker='o', linewidth=2)
    axes[1,0].set_title('Évolution Temporelle Annuelle')
    axes[1,0].set_xlabel('Année')
    axes[1,0].set_ylabel('Énergie Renouvelable Moyenne')
    axes[1,0].grid(True, alpha=0.3)

# Analyse saisonnière
if 'Month' in df.columns:
    monthly_trend = df.groupby('Month')['Total Renewable Energy'].mean()
    axes[1,1].bar(monthly_trend.index, monthly_trend.values, alpha=0.7, color='lightcoral')
    axes[1,1].set_title('Tendance Saisonnière')
    axes[1,1].set_xlabel('Mois')
    axes[1,1].set_ylabel('Énergie Renouvelable Moyenne')

plt.tight_layout()
plt.show()

# 2.2 Matrice de corrélation avancée
plt.figure(figsize=(14, 10))
corr_matrix = df.corr(numeric_only=True)
mask = np.triu(np.ones_like(corr_matrix, dtype=bool))
sns.heatmap(corr_matrix, mask=mask, annot=True, cmap='RdYlBu', center=0,
            square=True, fmt='.2f', cbar_kws={"shrink": .8})
plt.title('Matrice de Corrélation (Triangle Inférieur)', fontsize=14, fontweight='bold')
plt.tight_layout()
plt.show()

# 2.3 Analyse des corrélations fortes
print("\n🔗 CORRÉLATIONS FORTES avec la variable cible:")
target_corr = df.corr(numeric_only=True)['Total Renewable Energy'].abs().sort_values(ascending=False)
print(target_corr[target_corr > 0.3])

# 2.4 Graphiques de dispersion pour les variables les plus corrélées
top_corr_vars = target_corr[1:6].index  # Top 5 variables (excluant la cible)
fig, axes = plt.subplots(2, 3, figsize=(18, 12))
fig.suptitle('Relations entre Variables (Top Corrélations)', fontsize=16, fontweight='bold')

for i, var in enumerate(top_corr_vars):
    row, col = i // 3, i % 3
    if i < 5:
        axes[row, col].scatter(df[var], df['Total Renewable Energy'], alpha=0.6)
        axes[row, col].set_xlabel(var)
        axes[row, col].set_ylabel('Total Renewable Energy')
        
        # Ligne de régression
        z = np.polyfit(df[var], df['Total Renewable Energy'], 1)
        p = np.poly1d(z)
        axes[row, col].plot(df[var], p(df[var]), "r--", alpha=0.8)
        
        # Coefficient de corrélation
        corr_coef = df[var].corr(df['Total Renewable Energy'])
        axes[row, col].set_title(f'Corrélation: {corr_coef:.3f}')

# Supprimer le subplot vide
if len(top_corr_vars) < 6:
    fig.delaxes(axes[1, 2])

plt.tight_layout()
plt.show()

# =============================================================================
# 🤖 3. PRÉPARATION DES DONNÉES POUR ML
# =============================================================================

print("\n" + "="*60)
print("🤖 3. PRÉPARATION DES DONNÉES")
print("="*60)

# 3.1 Encodage des variables catégorielles (qualitatif en quantitatif)
df_ml = df.copy()

# Encodage du secteur
if 'Sector' in df_ml.columns:
    encoder = LabelEncoder()
    df_ml['Sector_encoded'] = encoder.fit_transform(df_ml['Sector'])
    print(f"✅ Encodage 'Sector': {dict(zip(encoder.classes_, encoder.transform(encoder.classes_)))}")

# 3.2 Ingénierie des caractéristiques (Feature Engineering)
print("\n🔧 INGÉNIERIE DES CARACTÉRISTIQUES")
print("-" * 40)

# Création de nouvelles variables
if 'Year' in df_ml.columns and 'Month' in df_ml.columns:
    df_ml['Year_Month'] = df_ml['Year'] * 100 + df_ml['Month']
    df_ml['Quarter'] = ((df_ml['Month'] - 1) // 3) + 1
    df_ml['Season'] = df_ml['Month'].map({12: 0, 1: 0, 2: 0,  # Hiver
                                         3: 1, 4: 1, 5: 1,   # Printemps
                                         6: 2, 7: 2, 8: 2,   # Été
                                         9: 3, 10: 3, 11: 3}) # Automne
    print("✅ Variables temporelles créées: Year_Month, Quarter, Season")

# Interactions entre variables
numeric_features = df_ml.select_dtypes(include=[np.number]).columns
if len(numeric_features) >= 2:
    # Exemple d'interaction entre les deux premières variables numériques
    feat1, feat2 = numeric_features[0], numeric_features[1]
    if feat1 != 'Total Renewable Energy' and feat2 != 'Total Renewable Energy':
        df_ml[f'{feat1}_x_{feat2}'] = df_ml[feat1] * df_ml[feat2]
        print(f"✅ Variable d'interaction créée: {feat1}_x_{feat2}")

# 3.3 Définition des variables
features = [col for col in df_ml.columns if col not in ['Total Renewable Energy', 'Sector']]
target = 'Total Renewable Energy'

print(f"\n📊 Variables explicatives ({len(features)}): {features}")
print(f"🎯 Variable cible: {target}")

X = df_ml[features]
y = df_ml[target]

# 3.4 Normalisation des données
scaler = StandardScaler()
X_scaled = pd.DataFrame(scaler.fit_transform(X), columns=X.columns, index=X.index)

print(f"\n✅ Normalisation effectuée (StandardScaler)")

# =============================================================================
# 🧠 4. SÉLECTION DES CARACTÉRISTIQUES
# =============================================================================

print("\n" + "="*60)
print("🧠 4. SÉLECTION DES CARACTÉRISTIQUES")
print("="*60)

# 4.1 Sélection univariée
selector = SelectKBest(score_func=f_regression, k=min(10, len(features)))
X_selected = selector.fit_transform(X_scaled, y)
selected_features = X.columns[selector.get_support()]

print(f"🎯 Sélection univariée - Top {len(selected_features)} caractéristiques:")
feature_scores = pd.DataFrame({
    'Feature': X.columns,
    'Score': selector.scores_,
    'Selected': selector.get_support()
}).sort_values('Score', ascending=False)
print(feature_scores.head(10))

# 4.2 Importance des caractéristiques avec Random Forest
rf_selector = RandomForestRegressor(n_estimators=100, random_state=42)
rf_selector.fit(X_scaled, y)
feature_importance = pd.DataFrame({
    'Feature': X.columns,
    'Importance': rf_selector.feature_importances_
}).sort_values('Importance', ascending=False)

print(f"\n🌲 Importance des caractéristiques (Random Forest):")
print(feature_importance.head(10))

# Visualisation de l'importance
plt.figure(figsize=(12, 8))
top_features = feature_importance.head(15)
plt.barh(range(len(top_features)), top_features['Importance'][::-1])
plt.yticks(range(len(top_features)), top_features['Feature'][::-1])
plt.xlabel('Importance')
plt.title('Top 15 - Importance des Caractéristiques (Random Forest)')
plt.tight_layout()
plt.show()

# =============================================================================
# 🤖 5. MODÉLISATION AVANCÉE
# =============================================================================

print("\n" + "="*60)
print("🤖 5. MODÉLISATION AVANCÉE")
print("="*60)

# 5.1 Division des données
X_train, X_test, y_train, y_test = train_test_split(
    X_scaled, y, test_size=0.2, random_state=42, stratify=None
)

print(f"📊 Données d'entraînement: {X_train.shape}")
print(f"📊 Données de test: {X_test.shape}")

# 5.2 Définition des modèles
models = {
    'Linear Regression': LinearRegression(),
    'Ridge Regression': Ridge(alpha=1.0),
    'Lasso Regression': Lasso(alpha=1.0),
    'Elastic Net': ElasticNet(alpha=1.0, l1_ratio=0.5),
    'Decision Tree': DecisionTreeRegressor(random_state=42),
    'Random Forest': RandomForestRegressor(n_estimators=100, random_state=42),
    'Extra Trees': ExtraTreesRegressor(n_estimators=100, random_state=42),
    'Gradient Boosting': GradientBoostingRegressor(n_estimators=100, random_state=42),
    'K-Neighbors': KNeighborsRegressor(n_neighbors=5),
    'Support Vector': SVR(kernel='rbf', C=1.0)
}

# 5.3 Entraînement et évaluation des modèles
print("\n🏃‍♂️ ENTRAÎNEMENT DES MODÈLES")
print("-" * 40)

results = []

for name, model in models.items():
    print(f"Entraînement: {name}...")
    
    # Validation croisée
    cv_scores = cross_val_score(model, X_train, y_train, cv=5, 
                               scoring='neg_mean_squared_error', n_jobs=-1)
    cv_rmse = np.sqrt(-cv_scores)
    
    # Entraînement sur toutes les données d'entraînement
    model.fit(X_train, y_train)
    
    # Prédictions
    y_pred_train = model.predict(X_train)
    y_pred_test = model.predict(X_test)
    
    # Métriques
    train_rmse = np.sqrt(mean_squared_error(y_train, y_pred_train))
    test_rmse = np.sqrt(mean_squared_error(y_test, y_pred_test))
    train_r2 = r2_score(y_train, y_pred_train)
    test_r2 = r2_score(y_test, y_pred_test)
    test_mae = mean_absolute_error(y_test, y_pred_test)
    
    results.append({
        'Modèle': name,
        'CV_RMSE_mean': cv_rmse.mean(),
        'CV_RMSE_std': cv_rmse.std(),
        'Train_RMSE': train_rmse,
        'Test_RMSE': test_rmse,
        'Train_R²': train_r2,
        'Test_R²': test_r2,
        'Test_MAE': test_mae,
        'Overfitting': train_rmse - test_rmse
    })

# 5.4 Compilation des résultats
results_df = pd.DataFrame(results)
results_df = results_df.sort_values('Test_R²', ascending=False)

print("\n📊 RÉSULTATS DES MODÈLES")
print("="*80)
print(results_df.round(4))

# 5.5 Optimisation du meilleur modèle
print("\n🎯 OPTIMISATION DU MEILLEUR MODÈLE")
print("-" * 40)

best_model_name = results_df.iloc[0]['Modèle']
print(f"Meilleur modèle: {best_model_name}")

# Optimisation des hyperparamètres pour Random Forest ou Gradient Boosting
if best_model_name in ['Random Forest', 'Gradient Boosting']:
    if best_model_name == 'Random Forest':
        param_grid = {
            'n_estimators': [50, 100, 200],
            'max_features': ['auto', 'sqrt', 'log2'],
            'max_depth': [None, 10, 20, 30],
            'min_samples_split': [2, 5, 10]
        }
        model_to_optimize = RandomForestRegressor(random_state=42)
    else:
        param_grid = {
            'n_estimators': [50, 100, 200],
            'learning_rate': [0.01, 0.1, 0.2],
            'max_depth': [3, 5, 7],
            'min_samples_split': [2, 5, 10]
        }
        model_to_optimize = GradientBoostingRegressor(random_state=42)
    
    print("🔍 Recherche des meilleurs hyperparamètres...")
    grid_search = GridSearchCV(
        model_to_optimize, param_grid, cv=3, 
        scoring='neg_mean_squared_error', n_jobs=-1
    )
    grid_search.fit(X_train, y_train)
    
    print(f"✅ Meilleurs paramètres: {grid_search.best_params_}")
    
    # Évaluation du modèle optimisé
    best_model_optimized = grid_search.best_estimator_
    y_pred_optimized = best_model_optimized.predict(X_test)
    
    optimized_rmse = np.sqrt(mean_squared_error(y_test, y_pred_optimized))
    optimized_r2 = r2_score(y_test, y_pred_optimized)
    
    print(f"📊 Performance optimisée - RMSE: {optimized_rmse:.4f}, R²: {optimized_r2:.4f}")

# =============================================================================
# 📈 6. ANALYSE DES RÉSULTATS ET INTERPRÉTATION
# =============================================================================

print("\n" + "="*60)
print("📈 6. ANALYSE DES RÉSULTATS")
print("="*60)

# 6.1 Visualisation des performances
fig, axes = plt.subplots(2, 2, figsize=(16, 12))
fig.suptitle('Analyse des Performances des Modèles', fontsize=16, fontweight='bold')

# R² Score
axes[0,0].barh(results_df['Modèle'], results_df['Test_R²'])
axes[0,0].set_title('R² Score (Test)')
axes[0,0].set_xlabel('R² Score')

# RMSE
axes[0,1].barh(results_df['Modèle'], results_df['Test_RMSE'])
axes[0,1].set_title('RMSE (Test)')
axes[0,1].set_xlabel('RMSE')

# Overfitting (différence Train-Test RMSE)
axes[1,0].barh(results_df['Modèle'], results_df['Overfitting'])
axes[1,0].set_title('Overfitting (Train RMSE - Test RMSE)')
axes[1,0].set_xlabel('Différence RMSE')
axes[1,0].axvline(x=0, color='red', linestyle='--', alpha=0.7)

# Validation croisée
axes[1,1].errorbar(results_df['CV_RMSE_mean'], range(len(results_df)), 
                   xerr=results_df['CV_RMSE_std'], fmt='o')
axes[1,1].set_yticks(range(len(results_df)))
axes[1,1].set_yticklabels(results_df['Modèle'])
axes[1,1].set_title('Validation Croisée (RMSE ± Std)')
axes[1,1].set_xlabel('RMSE')

plt.tight_layout()
plt.show()

# 6.2 Analyse des résidus pour le meilleur modèle
best_model = models[best_model_name]
best_model.fit(X_train, y_train)
y_pred_best = best_model.predict(X_test)
residuals = y_test - y_pred_best

fig, axes = plt.subplots(1, 3, figsize=(18, 6))
fig.suptitle(f'Analyse des Résidus - {best_model_name}', fontsize=16, fontweight='bold')

# Résidus vs Prédictions
axes[0].scatter(y_pred_best, residuals, alpha=0.6)
axes[0].axhline(y=0, color='red', linestyle='--')
axes[0].set_xlabel('Prédictions')
axes[0].set_ylabel('Résidus')
axes[0].set_title('Résidus vs Prédictions')

# Distribution des résidus
axes[1].hist(residuals, bins=20, alpha=0.7, density=True)
axes[1].set_xlabel('Résidus')
axes[1].set_ylabel('Densité')
axes[1].set_title('Distribution des Résidus')

# Q-Q plot
stats.probplot(residuals, dist="norm", plot=axes[2])
axes[2].set_title('Q-Q Plot (Normalité des Résidus)')

plt.tight_layout()
plt.show()

# 6.3 Valeurs réelles vs prédites
plt.figure(figsize=(10, 8))
plt.scatter(y_test, y_pred_best, alpha=0.6)
plt.plot([y_test.min(), y_test.max()], [y_test.min(), y_test.max()], 'r--', lw=2)
plt.xlabel('Valeurs Réelles')
plt.ylabel('Valeurs Prédites')
plt.title(f'Valeurs Réelles vs Prédites - {best_model_name}')
plt.grid(True, alpha=0.3)

# Coefficient de corrélation
corr_coef = np.corrcoef(y_test, y_pred_best)[0, 1]
plt.text(0.05, 0.95, f'Corrélation: {corr_coef:.3f}', 
         transform=plt.gca().transAxes, fontsize=12, fontweight='bold',
         bbox=dict(boxstyle='round', facecolor='wheat', alpha=0.8))

plt.tight_layout()
plt.show()

# =============================================================================
# 📋 7. RAPPORT FINAL
# =============================================================================

print("\n" + "="*80)
print("📋 RAPPORT FINAL - ANALYSE ÉNERGÉTIQUE USA")
print("="*80)

print(f"""
🔍 RÉSUMÉ DE L'ANALYSE
{"-"*50}
• Dataset: {df.shape[0]} observations, {df.shape[1]} variables
• Période: {df['Year'].min() if 'Year' in df.columns else 'N/A'} - {df['Year'].max() if 'Year' in df.columns else 'N/A'}
• Secteurs analysés: {df['Sector'].nunique() if 'Sector' in df.columns else 'N/A'}
• Variables créées: {len(features) - len(df.select_dtypes(include=[np.number]).columns) + 1}

🎯 VARIABLE CIBLE
{"-"*50}
• Nom: {target}
• Moyenne: {y.mean():.2f}
• Médiane: {y.median():.2f}
• Écart-type: {y.std():.2f}
• Min-Max: [{y.min():.2f}, {y.max():.2f}]

🏆 MEILLEUR MODÈLE
{"-"*50}
• Modèle: {best_model_name}
• R² (Test): {results_df.iloc[0]['Test_R²']:.4f}
• RMSE (Test): {results_df.iloc[0]['Test_RMSE']:.4f}
• MAE (Test): {results_df.iloc[0]['Test_MAE']:.4f}

🔝 TOP 5 MODÈLES
{"-"*50}""")

for i, row in results_df.head(5).iterrows():
    print(f"{i+1}. {row['Modèle']:<20} R²: {row['Test_R²']:.4f} | RMSE: {row['Test_RMSE']:.4f}")

print(f"""
🎯 TOP 5 CARACTÉRISTIQUES IMPORTANTES
{"-"*50}""")
for i, row in feature_importance.head(5).iterrows():
    print(f"{i+1}. {row['Feature']:<25} Importance: {row['Importance']:.4f}")

print(f"""
📊 CORRÉLATIONS FORTES (|r| > 0.3)
{"-"*50}""")
strong_corr = target_corr[target_corr > 0.3]
for var, corr in strong_corr.items():
    if var != target:
        print(f"• {var:<30} r = {corr:.3f}")

print(f"""
✅ CONCLUSIONS ET RECOMMANDATIONS
{"-"*50}
1. Le modèle {best_model_name} présente les meilleures performances avec un R² de {results_df.iloc[0]['Test_R²']:.3f}
2. Les {len(strong_corr)-1} variables les plus corrélées expliquent une grande partie de la variance
3. {"Peu de surapprentissage détecté" if abs(results_df.iloc[0]['Overfitting']) < 0.1 else "Surapprentissage modéré détecté"}
4. La qualité des données est {"excellente" if missing_data.sum() == 0 else "correcte avec quelques valeurs manquantes"}

🚀 PROCHAINES ÉTAPES
{"-"*50}
• Collecter plus de données pour améliorer la robustesse
• Tester des modèles d'ensemble plus sophistiqués
• Implémenter une pipeline de prédiction en temps réel
• Analyser l'impact des variables externes (météo, économie)
""")

print("="*80)
print("🎉 ANALYSE TERMINÉE AVEC SUCCÈS!")
print("="*80)