# tests/testthat/helper-mock-data.R

cat('Intitializing package for tests\n', file = stderr())

# Create a test DB with all the tables used in test
library(DBI)
library(RSQLite)

# Create in-memory database
CON <- dbConnect(SQLite(), ":memory:")

# === AUDIT LOG TABLE ===
dbExecute(
  CON,
  "
  CREATE TABLE audit_log (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    username TEXT NOT NULL,
    date_time TEXT NOT NULL,
    user_action TEXT NOT NULL
  )
  "
)

# === TRAINING TABLE ===
dbExecute(
  CON,
  "
  CREATE TABLE training (
    id INTEGER PRIMARY KEY,
    start_time TEXT,
    end_time TEXT,
    training_type TEXT
  )
  "
)

dbExecute(
  CON,
  "
  INSERT INTO training VALUES 
  (1, '2024-01-15 10:00:00', '2024-01-15 12:00:00', 'CPR'),
  (2, '2024-01-16 14:00:00', '2024-01-16 16:00:00', 'Fire Suppression')
  "
)

# === FIREFIGHTER TABLE ===
dbExecute(
  CON,
  "
  CREATE TABLE firefighter (
    id INTEGER PRIMARY KEY,
    name TEXT,
    display_order INTEGER,
    start_date TEXT
  )
  "
)

dbExecute(
  CON,
  "
  INSERT INTO firefighter VALUES 
  (1, 'John Doe', 2, '2020-01-15'),
  (2, 'Jane Smith', 1, '2019-06-01'),
  (3, 'Bob Johnson', 3, '2021-03-10')
  "
)

# === ATTENDANCE TABLE ===
dbExecute(
  CON,
  "
  CREATE TABLE attendance (
    id INTEGER PRIMARY KEY,
    firefighter_id INTEGER,
    check_in TEXT,
    check_out TEXT
  )
  "
)

dbExecute(
  CON,
  "
  INSERT INTO attendance VALUES 
  (1, 1, '2024-01-15 08:00:00', '2024-01-15 17:00:00'),
  (2, 2, '2024-01-15 08:00:00', '2024-01-15 17:00:00')
  "
)

# === FIREFIGHTER_RESPONSE TABLE ===
dbExecute(
  CON,
  "
  CREATE TABLE firefighter_response (
    id INTEGER PRIMARY KEY,
    firefighter_id INTEGER,
    incident_id INTEGER,
    time_adjustment REAL
  )
  "
)

dbExecute(
  CON,
  "
  INSERT INTO firefighter_response VALUES 
  (1, 1, 100, 5.5),
  (2, 2, 100, NULL),
  (3, 1, 101, 0)
  "
)

# === PATIENTS TABLE ===
dbExecute(
  CON,
  "
  CREATE TABLE patients (
    id INTEGER PRIMARY KEY,
    name TEXT NOT NULL,
    age INTEGER
  )
  "
)

dbExecute(
  CON,
  "
  INSERT INTO patients (id, name, age) VALUES 
  (1, 'John Doe', 45),
  (2, 'Jane Smith', 32),
  (3, 'Bob Johnson', 67)
  "
)

# === MEDICATIONS TABLE ===
dbExecute(
  CON,
  "
  CREATE TABLE medications (
    id INTEGER PRIMARY KEY,
    drug_name TEXT NOT NULL,
    dosage TEXT
  )
  "
)

dbExecute(
  CON,
  "
  INSERT INTO medications (id, drug_name, dosage) VALUES 
  (1, 'Aspirin', '100mg'),
  (2, 'Ibuprofen', '200mg')
  "
)

# === EMPTY TABLE ===
dbExecute(
  CON,
  "
  CREATE TABLE empty_table (
    id INTEGER,
    value TEXT
  )
  "
)

# === TEST_TABLE_123 ===
dbExecute(
  CON,
  "
  CREATE TABLE test_table_123 (
    id INTEGER
  )
  "
)

# === MAIN.TEST_SCHEMA_TABLE ===
dbExecute(
  CON,
  "
  CREATE TABLE main.test_schema_table (
    id INTEGER,
    value TEXT
  )
  "
)

dbExecute(
  CON,
  "
  INSERT INTO main.test_schema_table VALUES 
  (1, 'test')
  "
)

# === UNKNOWN_TABLE ===
dbExecute(
  CON,
  "
  CREATE TABLE unknown_table (
    id INTEGER,
    value TEXT
  )
  "
)

dbExecute(
  CON,
  "
  INSERT INTO unknown_table VALUES 
  (1, 'test')
  "
)

.GetMockAppData <- function() {
  list(
    Setting = data.frame(
      id = 1:23,
      domain = c(
        # Global settings (1-7)
        "global",
        "global",
        "global",
        "global",
        "global",
        "global",
        "global",
        # Incident settings (8-13)
        "incident",
        "incident",
        "incident",
        "incident",
        "incident",
        "incident",
        # Test settings for numeric (14)
        "test",
        # Test settings for date (15)
        "test",
        # Test settings for boolean variations (16-21)
        "test",
        "test",
        "test",
        "test",
        "test",
        "test",
        # Test settings for unknown type (22)
        "test",
        # Test settings for relative date (23)
        "test"
      ),
      setting_group = c(
        # Global
        "id",
        "time",
        "time",
        "time",
        "format",
        "id",
        "config",
        # Incident
        "incident_response",
        "incident_response",
        "incident_response",
        "incident_response",
        "incident_response",
        "incident_response",
        # Test
        "numbers",
        "dates",
        "bools",
        "bools",
        "bools",
        "bools",
        "bools",
        "bools",
        "misc",
        "dates"
      ),
      setting_key = c(
        # Global
        "password",
        "ltz",
        "date_format",
        "date_time_format",
        "sidebar_open",
        "fire_dept_id",
        "safe_delete",
        # Incident
        "input_secondary",
        "canceled",
        "dropped",
        "address",
        "incident_cad_regex",
        "cad_id",
        # Test
        "max_count",
        "start_date",
        "bool1",
        "bool2",
        "bool3",
        "bool4",
        "bool5",
        "bool6",
        "unknown_type",
        "relative"
      ),
      setting_value = c(
        # Global
        "123",
        "America/Denver",
        "%m-%d-%Y",
        "%m-%d-%Y %H:%M",
        "TRUE",
        "Crabaple",
        "FALSE",
        # Incident
        "FALSE",
        "TRUE",
        "TRUE",
        "TRUE",
        "(?:\\d[7])",
        "Must be exactly 9",
        # Test
        "42",
        "2025-01-15",
        "true",
        "TRUE",
        "1",
        "yes",
        "false",
        "0",
        "some_value",
        "today"
      ),
      value_type = c(
        # Global
        "string",
        "string",
        "string",
        "string",
        "boolean",
        "string",
        "boolean",
        # Incident
        "boolean",
        "boolean",
        "boolean",
        "boolean",
        "string",
        "string",
        # Test
        "numeric",
        "date",
        "boolean",
        "boolean",
        "boolean",
        "boolean",
        "boolean",
        "boolean",
        "unknown",
        "relative_date"
      ),
      description = c(
        # Global
        "This password",
        "Time zone",
        "Date format",
        "Date time format",
        "Will the sidebar",
        "Title, display name",
        "Will there be",
        # Incident
        "Are secondary",
        "Is the canceled",
        "Is the dropdown",
        "Is the address",
        "Regex for category",
        "Message to show",
        # Test
        "Test numeric",
        "Test date",
        "Test bool",
        "Test bool",
        "Test bool",
        "Test bool",
        "Test bool",
        "Test bool",
        "Test unknown",
        "Test relative date"
      ),
      stringsAsFactors = FALSE
    ),
    CON = CON,
    current_local_date = Sys.time() |> with_tz('America/Denver') |> as.Date()
  )
}

# Initialize once for all tests
InitializePackage(.GetMockAppData())
