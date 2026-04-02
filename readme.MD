$ python -m venv .venv
source .venv/Scripts/activate
cd API_Team
pip install -r requirements.txt
pip list
uvicorn src.main:app --reload
