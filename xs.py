from matplotlib.colors import ListedColormap
from sklearn import linear_model
from scipy.special import expit
# Fit the classifier
clf = linear_model.LogisticRegression()
dt=pd.DataFrame({'binar':y_tr, 
                 'var':train_data.loan_amnt.values})
dt.loc[y_tr==0,'color']='green'
dt.loc[y_tr>0,'color']='red'
clf.fit(dt['var'].values.reshape(-1,1), y_tr.values.reshape(-1,1))
dt['loss']  = expit(dt['var'].values * clf.coef_ + clf.intercept_).ravel()
dt = dt.sort_values('var')
colors = [(143/255., 188/255., 143/255.), (1., 1., 1.),  (216/255., 24/255., 24/255.),]

cmap = ListedColormap(colors)
ax = dt.plot.scatter('var','binar', c=dt['color'], figsize=(15, 8))
ax.plot(dt['var'].values.reshape(-1,1),dt['loss'], linewidth=1)
ax.pcolorfast(ax.get_xlim(), ax.get_ylim(),dt['binar'].values[np.newaxis],cmap=cmap, alpha=0.3)
ax.axis([dt['var'].min(), dt['var'].max(), dt['binar'].min(), dt['binar'].max()])
