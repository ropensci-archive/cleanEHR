/************ Update: Tables ***************/
--ALTER TABLE "episodes" DROP CONSTRAINT IF EXISTS pkepisodes;
DROP TABLE "demographic";
DROP TABLE "episodes";
DROP TABLE "physiology";
/******************** Add Table: "episodes" ************************/
CREATE SEQUENCE "episode_seq" INCREMENT BY 1;
CREATE SEQUENCE "demographic_seq" INCREMENT BY 1;
CREATE SEQUENCE "physiology_seq" INCREMENT BY 1;

/* Build Table Structure */
CREATE TABLE "episodes"
(
  "Id" INTEGER DEFAULT nextval('episode_seq'::regclass) NOT NULL,
  nhs_number CHAR(200) NULL,
  pas_number CHAR(200) NULL,
  site_id CHAR(200) NULL,
  local_id CHAR(200) NULL
);

/* Add Primary Key */
ALTER TABLE "episodes" ADD CONSTRAINT pkepisodes
PRIMARY KEY ("Id");


/******************** Add Table: "demographic" ************************/

/* Build Table Structure */
CREATE TABLE "demographic"
(
  "Id" INTEGER DEFAULT nextval('demographic_seq'::regclass) NOT NULL,
  item CHAR(100) NOT NULL,
  value CHAR(100) NULL,
  episode_id INTEGER NOT NULL


);

/* Add Primary Key */
ALTER TABLE "demographic" ADD CONSTRAINT pkdemographic
PRIMARY KEY ("Id");




/******************** Add Table: "physiology" ************************/

/* Build Table Structure */
CREATE TABLE "physiology"
(
  "Id" INTEGER DEFAULT nextval('physiology_seq'::regclass) NOT NULL,
  item CHAR(100) NOT NULL,
  value CHAR(100) NULL,
  episode_id INTEGER NOT NULL,
  time_stamp TIMESTAMP NOT NULL
);

/* Add Primary Key */
ALTER TABLE "physiology" ADD CONSTRAINT pkphysiology
PRIMARY KEY ("Id");



/************ Add Foreign Keys ***************/

/* Add Foreign Key: fk_demographic_episodes */
ALTER TABLE "demographic" ADD CONSTRAINT fk_demographic_episodes
FOREIGN KEY (episode_id) REFERENCES "episodes" ("Id")
ON UPDATE NO ACTION ON DELETE NO ACTION;
