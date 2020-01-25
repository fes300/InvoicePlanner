CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

CREATE TABLE users
(
  registration_date TIMESTAMPTZ NOT NULL DEFAULT current_timestamp,
  email TEXT NOT NULL,
  password TEXT NOT NULL,
  uuid_ uuid NOT NULL DEFAULT uuid_generate_v1(),
  CONSTRAINT pet_pkey PRIMARY KEY (uuid_)
)
