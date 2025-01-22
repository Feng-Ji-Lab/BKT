
from pyBKT.models import Model

print("123")
file_path = "simulation_data.csv"
defaults = {'order_id': 'order_id', 'user_id': 'student_id', 'correct': 'correct', 'skill_name': 'skill_name'}


model = Model(seed = 42)

print("123")
model.fit(data_path = file_path, defaults = defaults)
# results = model.predict(data)
print("123")
print(model.params())