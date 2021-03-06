/******************************************************************************/
/***          Generated by IBExpert 2018.1.6.1 28.10.2018 11:49:39          ***/
/******************************************************************************/

SET SQL DIALECT 3;

SET NAMES UTF8;

SET CLIENTLIB 'C:\Program Files (x86)\HK-Software\IBExpert\fbclient.dll';

CREATE DATABASE 'localhost/3050:D:\Projects\Lazarus\wFrameWork\demos\example\FDATABASE.FDB'
USER 'SYSDBA' PASSWORD 'masterkey'
PAGE_SIZE 4096
DEFAULT CHARACTER SET UTF8 COLLATION UTF8;



/******************************************************************************/
/***                                Domains                                 ***/
/******************************************************************************/

CREATE DOMAIN GUID AS
CHAR(16) CHARACTER SET OCTETS;



/******************************************************************************/
/***                               Generators                               ***/
/******************************************************************************/

CREATE GENERATOR GEN_DEPARTMENTS_ID START WITH 0 INCREMENT BY 1;
SET GENERATOR GEN_DEPARTMENTS_ID TO 9;

CREATE GENERATOR GEN_WORKERS_ID START WITH 0 INCREMENT BY 1;
SET GENERATOR GEN_WORKERS_ID TO 8;



/******************************************************************************/
/***                                 Tables                                 ***/
/******************************************************************************/



CREATE TABLE DEPARTMENTS (
    ID        BIGINT NOT NULL,
    IDPARENT  BIGINT,
    NAME      VARCHAR(500)
);

CREATE TABLE WORKERS (
    ID        BIGINT NOT NULL,
    IDPARENT  BIGINT,
    NAME      VARCHAR(500)
);

INSERT INTO DEPARTMENTS (ID, IDPARENT, NAME) VALUES (1, 0, 'Departments');
INSERT INTO DEPARTMENTS (ID, IDPARENT, NAME) VALUES (2, 1, 'Purchasing');
INSERT INTO DEPARTMENTS (ID, IDPARENT, NAME) VALUES (3, 1, 'Delivery');
INSERT INTO DEPARTMENTS (ID, IDPARENT, NAME) VALUES (4, 1, 'Sales');
INSERT INTO DEPARTMENTS (ID, IDPARENT, NAME) VALUES (5, 1, 'Best');
INSERT INTO DEPARTMENTS (ID, IDPARENT, NAME) VALUES (6, 5, 'Best 2');

COMMIT WORK;

INSERT INTO WORKERS (ID, IDPARENT, NAME) VALUES (1, 2, 'Julia');
INSERT INTO WORKERS (ID, IDPARENT, NAME) VALUES (2, 2, 'Artem');
INSERT INTO WORKERS (ID, IDPARENT, NAME) VALUES (3, 3, 'Ben');
INSERT INTO WORKERS (ID, IDPARENT, NAME) VALUES (4, 3, 'Liza');
INSERT INTO WORKERS (ID, IDPARENT, NAME) VALUES (5, 3, 'Margaret');
INSERT INTO WORKERS (ID, IDPARENT, NAME) VALUES (6, 3, 'Jurii');
INSERT INTO WORKERS (ID, IDPARENT, NAME) VALUES (7, 4, 'Alex');
INSERT INTO WORKERS (ID, IDPARENT, NAME) VALUES (8, 4, 'Max');

COMMIT WORK;

INSERT INTO DEPARTMENTS (ID, IDPARENT, NAME) VALUES (1, 0, 'Departments');
INSERT INTO DEPARTMENTS (ID, IDPARENT, NAME) VALUES (2, 1, 'Purchasing');
INSERT INTO DEPARTMENTS (ID, IDPARENT, NAME) VALUES (3, 1, 'Delivery');
INSERT INTO DEPARTMENTS (ID, IDPARENT, NAME) VALUES (4, 1, 'Sales');
INSERT INTO DEPARTMENTS (ID, IDPARENT, NAME) VALUES (5, 1, 'Best');
INSERT INTO DEPARTMENTS (ID, IDPARENT, NAME) VALUES (6, 5, 'Best 2');

COMMIT WORK;

INSERT INTO WORKERS (ID, IDPARENT, NAME) VALUES (1, 2, 'Julia');
INSERT INTO WORKERS (ID, IDPARENT, NAME) VALUES (2, 2, 'Artem');
INSERT INTO WORKERS (ID, IDPARENT, NAME) VALUES (3, 3, 'Ben');
INSERT INTO WORKERS (ID, IDPARENT, NAME) VALUES (4, 3, 'Liza');
INSERT INTO WORKERS (ID, IDPARENT, NAME) VALUES (5, 3, 'Margaret');
INSERT INTO WORKERS (ID, IDPARENT, NAME) VALUES (6, 3, 'Jurii');
INSERT INTO WORKERS (ID, IDPARENT, NAME) VALUES (7, 4, 'Alex');
INSERT INTO WORKERS (ID, IDPARENT, NAME) VALUES (8, 4, 'Max');

COMMIT WORK;

INSERT INTO DEPARTMENTS (ID, IDPARENT, NAME) VALUES (1, 0, 'Departments');
INSERT INTO DEPARTMENTS (ID, IDPARENT, NAME) VALUES (2, 1, 'Purchasing');
INSERT INTO DEPARTMENTS (ID, IDPARENT, NAME) VALUES (3, 1, 'Delivery');
INSERT INTO DEPARTMENTS (ID, IDPARENT, NAME) VALUES (4, 1, 'Sales');
INSERT INTO DEPARTMENTS (ID, IDPARENT, NAME) VALUES (5, 1, 'Best');
INSERT INTO DEPARTMENTS (ID, IDPARENT, NAME) VALUES (6, 5, 'Best 2');

COMMIT WORK;

INSERT INTO WORKERS (ID, IDPARENT, NAME) VALUES (1, 2, 'Julia');
INSERT INTO WORKERS (ID, IDPARENT, NAME) VALUES (2, 2, 'Artem');
INSERT INTO WORKERS (ID, IDPARENT, NAME) VALUES (3, 3, 'Ben');
INSERT INTO WORKERS (ID, IDPARENT, NAME) VALUES (4, 3, 'Liza');
INSERT INTO WORKERS (ID, IDPARENT, NAME) VALUES (5, 3, 'Margaret');
INSERT INTO WORKERS (ID, IDPARENT, NAME) VALUES (6, 3, 'Jurii');
INSERT INTO WORKERS (ID, IDPARENT, NAME) VALUES (7, 4, 'Alex');
INSERT INTO WORKERS (ID, IDPARENT, NAME) VALUES (8, 4, 'Max');

COMMIT WORK;

INSERT INTO DEPARTMENTS (ID, IDPARENT, NAME) VALUES (1, 0, 'Departments');
INSERT INTO DEPARTMENTS (ID, IDPARENT, NAME) VALUES (2, 1, 'Purchasing');
INSERT INTO DEPARTMENTS (ID, IDPARENT, NAME) VALUES (3, 1, 'Delivery');
INSERT INTO DEPARTMENTS (ID, IDPARENT, NAME) VALUES (4, 1, 'Sales');
INSERT INTO DEPARTMENTS (ID, IDPARENT, NAME) VALUES (5, 1, 'Best');
INSERT INTO DEPARTMENTS (ID, IDPARENT, NAME) VALUES (6, 5, 'Best 2');

COMMIT WORK;

INSERT INTO WORKERS (ID, IDPARENT, NAME) VALUES (1, 2, 'Julia');
INSERT INTO WORKERS (ID, IDPARENT, NAME) VALUES (2, 2, 'Artem');
INSERT INTO WORKERS (ID, IDPARENT, NAME) VALUES (3, 3, 'Ben');
INSERT INTO WORKERS (ID, IDPARENT, NAME) VALUES (4, 3, 'Liza');
INSERT INTO WORKERS (ID, IDPARENT, NAME) VALUES (5, 3, 'Margaret');
INSERT INTO WORKERS (ID, IDPARENT, NAME) VALUES (6, 3, 'Jurii');
INSERT INTO WORKERS (ID, IDPARENT, NAME) VALUES (7, 4, 'Alex');
INSERT INTO WORKERS (ID, IDPARENT, NAME) VALUES (8, 4, 'Max');

COMMIT WORK;



/******************************************************************************/
/***                              Primary keys                              ***/
/******************************************************************************/

ALTER TABLE WORKERS ADD CONSTRAINT PK_WORKERS PRIMARY KEY (ID);


/******************************************************************************/
/***                                Triggers                                ***/
/******************************************************************************/



SET TERM ^ ;



/******************************************************************************/
/***                          Triggers for tables                           ***/
/******************************************************************************/



/* Trigger: DEPARTMENTS_BI */
CREATE TRIGGER DEPARTMENTS_BI FOR DEPARTMENTS
ACTIVE BEFORE INSERT POSITION 0
AS  BEGIN    IF (NEW.ID IS NULL) THEN      NEW.ID = GEN_ID(GEN_DEPARTMENTS_ID,1);  END
^

/* Trigger: WORKERS_BI */
CREATE TRIGGER WORKERS_BI FOR WORKERS
ACTIVE BEFORE INSERT POSITION 0
AS  BEGIN    IF (NEW.ID IS NULL) THEN      NEW.ID = GEN_ID(GEN_WORKERS_ID,1);  END
^
SET TERM ; ^

