--
-- PostgreSQL database dump
--

-- Started on 2013-02-18 18:59:04 EST

SET statement_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = off;
SET check_function_bodies = false;
SET client_min_messages = warning;
SET escape_string_warning = off;

SET search_path = public, pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- TOC entry 140 (class 1259 OID 16653)
-- Dependencies: 3
-- Name: assignments; Type: TABLE; Schema: public; Owner: arcfide; Tablespace: 
--

CREATE TABLE assignments (
    name text NOT NULL
);


ALTER TABLE public.assignments OWNER TO arcfide;

--
-- TOC entry 141 (class 1259 OID 16659)
-- Dependencies: 3
-- Name: belongsto; Type: TABLE; Schema: public; Owner: arcfide; Tablespace: 
--

CREATE TABLE belongsto (
    assignment text NOT NULL,
    group_name text NOT NULL
);


ALTER TABLE public.belongsto OWNER TO arcfide;

--
-- TOC entry 142 (class 1259 OID 16665)
-- Dependencies: 3
-- Name: categories; Type: TABLE; Schema: public; Owner: arcfide; Tablespace: 
--

CREATE TABLE categories (
    name text NOT NULL
);


ALTER TABLE public.categories OWNER TO arcfide;

--
-- TOC entry 143 (class 1259 OID 16671)
-- Dependencies: 3
-- Name: collaborated; Type: TABLE; Schema: public; Owner: arcfide; Tablespace: 
--

CREATE TABLE collaborated (
    collaborator text NOT NULL,
    owner text NOT NULL,
    assignment text NOT NULL,
    date timestamp without time zone NOT NULL
);


ALTER TABLE public.collaborated OWNER TO arcfide;

--
-- TOC entry 144 (class 1259 OID 16677)
-- Dependencies: 3
-- Name: comments; Type: TABLE; Schema: public; Owner: arcfide; Tablespace: 
--

CREATE TABLE comments (
    comment_text text NOT NULL
);


ALTER TABLE public.comments OWNER TO arcfide;

--
-- TOC entry 145 (class 1259 OID 16683)
-- Dependencies: 3
-- Name: commentson; Type: TABLE; Schema: public; Owner: arcfide; Tablespace: 
--

CREATE TABLE commentson (
    comment text NOT NULL,
    problem text NOT NULL,
    com_start integer NOT NULL,
    com_end integer NOT NULL,
    owner text NOT NULL,
    assignment text NOT NULL,
    date timestamp without time zone NOT NULL
);


ALTER TABLE public.commentson OWNER TO arcfide;

--
-- TOC entry 146 (class 1259 OID 16689)
-- Dependencies: 3
-- Name: contains; Type: TABLE; Schema: public; Owner: arcfide; Tablespace: 
--

CREATE TABLE contains (
    assignment text NOT NULL,
    problem text NOT NULL,
    number text NOT NULL
);


ALTER TABLE public.contains OWNER TO arcfide;

--
-- TOC entry 147 (class 1259 OID 16695)
-- Dependencies: 3
-- Name: deadlines; Type: TABLE; Schema: public; Owner: arcfide; Tablespace: 
--

CREATE TABLE deadlines (
    type text NOT NULL,
    date timestamp without time zone NOT NULL
);


ALTER TABLE public.deadlines OWNER TO arcfide;

--
-- TOC entry 148 (class 1259 OID 16701)
-- Dependencies: 3
-- Name: groups; Type: TABLE; Schema: public; Owner: arcfide; Tablespace: 
--

CREATE TABLE groups (
    name text NOT NULL
);


ALTER TABLE public.groups OWNER TO arcfide;

--
-- TOC entry 149 (class 1259 OID 16707)
-- Dependencies: 3
-- Name: hasdeadline; Type: TABLE; Schema: public; Owner: arcfide; Tablespace: 
--

CREATE TABLE hasdeadline (
    assignment text NOT NULL,
    type text NOT NULL,
    date timestamp without time zone NOT NULL
);


ALTER TABLE public.hasdeadline OWNER TO arcfide;

--
-- TOC entry 150 (class 1259 OID 16713)
-- Dependencies: 3
-- Name: hastype; Type: TABLE; Schema: public; Owner: arcfide; Tablespace: 
--

CREATE TABLE hastype (
    assignment text NOT NULL,
    category text NOT NULL
);


ALTER TABLE public.hastype OWNER TO arcfide;

--
-- TOC entry 151 (class 1259 OID 16719)
-- Dependencies: 3
-- Name: instructors; Type: TABLE; Schema: public; Owner: arcfide; Tablespace: 
--

CREATE TABLE instructors (
    networkid text NOT NULL,
    firstname text NOT NULL,
    lastname text NOT NULL
);


ALTER TABLE public.instructors OWNER TO arcfide;

--
-- TOC entry 152 (class 1259 OID 16725)
-- Dependencies: 3
-- Name: memberof; Type: TABLE; Schema: public; Owner: arcfide; Tablespace: 
--

CREATE TABLE memberof (
    group_name text NOT NULL,
    student text NOT NULL
);


ALTER TABLE public.memberof OWNER TO arcfide;

--
-- TOC entry 153 (class 1259 OID 16731)
-- Dependencies: 1860 3
-- Name: problems; Type: TABLE; Schema: public; Owner: arcfide; Tablespace: 
--

CREATE TABLE problems (
    name text NOT NULL,
    description text DEFAULT 'No description given.'::text NOT NULL,
    grader text NOT NULL,
    grader_params text NOT NULL,
    testsuite text NOT NULL,
    solution text NOT NULL
);


ALTER TABLE public.problems OWNER TO arcfide;

--
-- TOC entry 154 (class 1259 OID 16738)
-- Dependencies: 1861 3
-- Name: submissions; Type: TABLE; Schema: public; Owner: arcfide; Tablespace: 
--

CREATE TABLE submissions (
    owner text NOT NULL,
    submittedfor text NOT NULL,
    date timestamp without time zone NOT NULL,
    isappeal boolean DEFAULT false NOT NULL,
    code bytea[] NOT NULL,
    report xml
);


ALTER TABLE public.submissions OWNER TO arcfide;

--
-- TOC entry 155 (class 1259 OID 16745)
-- Dependencies: 3
-- Name: submitters; Type: TABLE; Schema: public; Owner: arcfide; Tablespace: 
--

CREATE TABLE submitters (
    networkid text NOT NULL,
    firstname text NOT NULL,
    lastname text NOT NULL
);


ALTER TABLE public.submitters OWNER TO arcfide;

--
-- TOC entry 156 (class 1259 OID 16751)
-- Dependencies: 3
-- Name: teaches; Type: TABLE; Schema: public; Owner: arcfide; Tablespace: 
--

CREATE TABLE teaches (
    teacher text NOT NULL,
    group_name text NOT NULL
);


ALTER TABLE public.teaches OWNER TO arcfide;

--
-- TOC entry 157 (class 1259 OID 16757)
-- Dependencies: 3
-- Name: validates; Type: TABLE; Schema: public; Owner: arcfide; Tablespace: 
--

CREATE TABLE validates (
    validator text NOT NULL,
    assignment text NOT NULL,
    params text NOT NULL
);


ALTER TABLE public.validates OWNER TO arcfide;

--
-- TOC entry 158 (class 1259 OID 16763)
-- Dependencies: 3
-- Name: validators; Type: TABLE; Schema: public; Owner: arcfide; Tablespace: 
--

CREATE TABLE validators (
    name text NOT NULL,
    command text NOT NULL
);


ALTER TABLE public.validators OWNER TO arcfide;

--
-- TOC entry 1863 (class 2606 OID 16770)
-- Dependencies: 140 140
-- Name: assignments_key; Type: CONSTRAINT; Schema: public; Owner: arcfide; Tablespace: 
--

ALTER TABLE ONLY assignments
    ADD CONSTRAINT assignments_key PRIMARY KEY (name);


--
-- TOC entry 1865 (class 2606 OID 16772)
-- Dependencies: 141 141 141
-- Name: belongsto_key; Type: CONSTRAINT; Schema: public; Owner: arcfide; Tablespace: 
--

ALTER TABLE ONLY belongsto
    ADD CONSTRAINT belongsto_key PRIMARY KEY (assignment, group_name);


--
-- TOC entry 1867 (class 2606 OID 16774)
-- Dependencies: 142 142
-- Name: categories_key; Type: CONSTRAINT; Schema: public; Owner: arcfide; Tablespace: 
--

ALTER TABLE ONLY categories
    ADD CONSTRAINT categories_key PRIMARY KEY (name);


--
-- TOC entry 1869 (class 2606 OID 16776)
-- Dependencies: 143 143 143 143 143
-- Name: collaborated_key; Type: CONSTRAINT; Schema: public; Owner: arcfide; Tablespace: 
--

ALTER TABLE ONLY collaborated
    ADD CONSTRAINT collaborated_key PRIMARY KEY (collaborator, owner, assignment, date);


--
-- TOC entry 1871 (class 2606 OID 16778)
-- Dependencies: 144 144
-- Name: comments_key; Type: CONSTRAINT; Schema: public; Owner: arcfide; Tablespace: 
--

ALTER TABLE ONLY comments
    ADD CONSTRAINT comments_key PRIMARY KEY (comment_text);


--
-- TOC entry 1873 (class 2606 OID 16780)
-- Dependencies: 145 145 145 145 145 145 145 145
-- Name: commentson_key; Type: CONSTRAINT; Schema: public; Owner: arcfide; Tablespace: 
--

ALTER TABLE ONLY commentson
    ADD CONSTRAINT commentson_key PRIMARY KEY (comment, problem, com_start, com_end, owner, assignment, date);


--
-- TOC entry 1876 (class 2606 OID 16782)
-- Dependencies: 146 146 146
-- Name: contains_key; Type: CONSTRAINT; Schema: public; Owner: arcfide; Tablespace: 
--

ALTER TABLE ONLY contains
    ADD CONSTRAINT contains_key PRIMARY KEY (assignment, problem);


--
-- TOC entry 1878 (class 2606 OID 16784)
-- Dependencies: 147 147 147
-- Name: deadlines_key; Type: CONSTRAINT; Schema: public; Owner: arcfide; Tablespace: 
--

ALTER TABLE ONLY deadlines
    ADD CONSTRAINT deadlines_key PRIMARY KEY (type, date);


--
-- TOC entry 1880 (class 2606 OID 16786)
-- Dependencies: 148 148
-- Name: groups_key; Type: CONSTRAINT; Schema: public; Owner: arcfide; Tablespace: 
--

ALTER TABLE ONLY groups
    ADD CONSTRAINT groups_key PRIMARY KEY (name);


--
-- TOC entry 1882 (class 2606 OID 16788)
-- Dependencies: 149 149 149 149
-- Name: hasdeadline_key; Type: CONSTRAINT; Schema: public; Owner: arcfide; Tablespace: 
--

ALTER TABLE ONLY hasdeadline
    ADD CONSTRAINT hasdeadline_key PRIMARY KEY (assignment, type, date);


--
-- TOC entry 1884 (class 2606 OID 16790)
-- Dependencies: 150 150 150
-- Name: hastype_key; Type: CONSTRAINT; Schema: public; Owner: arcfide; Tablespace: 
--

ALTER TABLE ONLY hastype
    ADD CONSTRAINT hastype_key PRIMARY KEY (assignment, category);


--
-- TOC entry 1886 (class 2606 OID 16792)
-- Dependencies: 151 151
-- Name: instructors_key; Type: CONSTRAINT; Schema: public; Owner: arcfide; Tablespace: 
--

ALTER TABLE ONLY instructors
    ADD CONSTRAINT instructors_key PRIMARY KEY (networkid);


--
-- TOC entry 1888 (class 2606 OID 16794)
-- Dependencies: 152 152 152
-- Name: memberof_key; Type: CONSTRAINT; Schema: public; Owner: arcfide; Tablespace: 
--

ALTER TABLE ONLY memberof
    ADD CONSTRAINT memberof_key PRIMARY KEY (group_name, student);


--
-- TOC entry 1890 (class 2606 OID 16796)
-- Dependencies: 153 153
-- Name: problems_key; Type: CONSTRAINT; Schema: public; Owner: arcfide; Tablespace: 
--

ALTER TABLE ONLY problems
    ADD CONSTRAINT problems_key PRIMARY KEY (name);


--
-- TOC entry 1892 (class 2606 OID 16798)
-- Dependencies: 154 154 154 154
-- Name: submissions_key; Type: CONSTRAINT; Schema: public; Owner: arcfide; Tablespace: 
--

ALTER TABLE ONLY submissions
    ADD CONSTRAINT submissions_key PRIMARY KEY (owner, submittedfor, date);


--
-- TOC entry 1894 (class 2606 OID 16800)
-- Dependencies: 155 155
-- Name: submitters_key; Type: CONSTRAINT; Schema: public; Owner: arcfide; Tablespace: 
--

ALTER TABLE ONLY submitters
    ADD CONSTRAINT submitters_key PRIMARY KEY (networkid);


--
-- TOC entry 1896 (class 2606 OID 16802)
-- Dependencies: 156 156 156
-- Name: teaches_key; Type: CONSTRAINT; Schema: public; Owner: arcfide; Tablespace: 
--

ALTER TABLE ONLY teaches
    ADD CONSTRAINT teaches_key PRIMARY KEY (teacher, group_name);


--
-- TOC entry 1898 (class 2606 OID 16804)
-- Dependencies: 157 157 157 157
-- Name: validates_key; Type: CONSTRAINT; Schema: public; Owner: arcfide; Tablespace: 
--

ALTER TABLE ONLY validates
    ADD CONSTRAINT validates_key PRIMARY KEY (validator, assignment, params);


--
-- TOC entry 1900 (class 2606 OID 16806)
-- Dependencies: 158 158
-- Name: validators_key; Type: CONSTRAINT; Schema: public; Owner: arcfide; Tablespace: 
--

ALTER TABLE ONLY validators
    ADD CONSTRAINT validators_key PRIMARY KEY (name);


--
-- TOC entry 1874 (class 1259 OID 16807)
-- Dependencies: 145 145
-- Name: fki_commentson_contains_forkey; Type: INDEX; Schema: public; Owner: arcfide; Tablespace: 
--

CREATE INDEX fki_commentson_contains_forkey ON commentson USING btree (assignment, problem);


--
-- TOC entry 1901 (class 2606 OID 16808)
-- Dependencies: 1862 140 141
-- Name: belongsto_assignments_forkey; Type: FK CONSTRAINT; Schema: public; Owner: arcfide
--

ALTER TABLE ONLY belongsto
    ADD CONSTRAINT belongsto_assignments_forkey FOREIGN KEY (assignment) REFERENCES assignments(name);


--
-- TOC entry 1902 (class 2606 OID 16813)
-- Dependencies: 148 141 1879
-- Name: belongsto_groups_forkey; Type: FK CONSTRAINT; Schema: public; Owner: arcfide
--

ALTER TABLE ONLY belongsto
    ADD CONSTRAINT belongsto_groups_forkey FOREIGN KEY (group_name) REFERENCES groups(name);


--
-- TOC entry 1903 (class 2606 OID 16818)
-- Dependencies: 154 143 143 143 154 154 1891
-- Name: collaborated_submissions_forkey; Type: FK CONSTRAINT; Schema: public; Owner: arcfide
--

ALTER TABLE ONLY collaborated
    ADD CONSTRAINT collaborated_submissions_forkey FOREIGN KEY (owner, assignment, date) REFERENCES submissions(owner, submittedfor, date);


--
-- TOC entry 1904 (class 2606 OID 16823)
-- Dependencies: 1893 155 143
-- Name: collaborated_submitters_forkey; Type: FK CONSTRAINT; Schema: public; Owner: arcfide
--

ALTER TABLE ONLY collaborated
    ADD CONSTRAINT collaborated_submitters_forkey FOREIGN KEY (collaborator) REFERENCES submitters(networkid);


--
-- TOC entry 1905 (class 2606 OID 16828)
-- Dependencies: 144 145 1870
-- Name: commentson_comments_forkey; Type: FK CONSTRAINT; Schema: public; Owner: arcfide
--

ALTER TABLE ONLY commentson
    ADD CONSTRAINT commentson_comments_forkey FOREIGN KEY (comment) REFERENCES comments(comment_text);


--
-- TOC entry 1906 (class 2606 OID 16833)
-- Dependencies: 146 145 145 146 1875
-- Name: commentson_contains_forkey; Type: FK CONSTRAINT; Schema: public; Owner: arcfide
--

ALTER TABLE ONLY commentson
    ADD CONSTRAINT commentson_contains_forkey FOREIGN KEY (assignment, problem) REFERENCES contains(assignment, problem);


--
-- TOC entry 1907 (class 2606 OID 16838)
-- Dependencies: 145 1889 153
-- Name: commentson_problems_forkey; Type: FK CONSTRAINT; Schema: public; Owner: arcfide
--

ALTER TABLE ONLY commentson
    ADD CONSTRAINT commentson_problems_forkey FOREIGN KEY (problem) REFERENCES problems(name);


--
-- TOC entry 1908 (class 2606 OID 16843)
-- Dependencies: 145 145 145 154 154 154 1891
-- Name: commentson_submissions_forkey; Type: FK CONSTRAINT; Schema: public; Owner: arcfide
--

ALTER TABLE ONLY commentson
    ADD CONSTRAINT commentson_submissions_forkey FOREIGN KEY (owner, assignment, date) REFERENCES submissions(owner, submittedfor, date);


--
-- TOC entry 1909 (class 2606 OID 16848)
-- Dependencies: 140 146 1862
-- Name: contains_assignments_forkey; Type: FK CONSTRAINT; Schema: public; Owner: arcfide
--

ALTER TABLE ONLY contains
    ADD CONSTRAINT contains_assignments_forkey FOREIGN KEY (assignment) REFERENCES assignments(name);


--
-- TOC entry 1910 (class 2606 OID 16853)
-- Dependencies: 146 153 1889
-- Name: contains_problems_forkey; Type: FK CONSTRAINT; Schema: public; Owner: arcfide
--

ALTER TABLE ONLY contains
    ADD CONSTRAINT contains_problems_forkey FOREIGN KEY (problem) REFERENCES problems(name);


--
-- TOC entry 1911 (class 2606 OID 16858)
-- Dependencies: 1862 149 140
-- Name: hasdeadline_assignments_forkey; Type: FK CONSTRAINT; Schema: public; Owner: arcfide
--

ALTER TABLE ONLY hasdeadline
    ADD CONSTRAINT hasdeadline_assignments_forkey FOREIGN KEY (assignment) REFERENCES assignments(name);


--
-- TOC entry 1912 (class 2606 OID 16863)
-- Dependencies: 149 149 147 1877 147
-- Name: hasdeadline_deadlines_forkey; Type: FK CONSTRAINT; Schema: public; Owner: arcfide
--

ALTER TABLE ONLY hasdeadline
    ADD CONSTRAINT hasdeadline_deadlines_forkey FOREIGN KEY (type, date) REFERENCES deadlines(type, date);


--
-- TOC entry 1913 (class 2606 OID 16868)
-- Dependencies: 140 150 1862
-- Name: hastype_assignments_forkey; Type: FK CONSTRAINT; Schema: public; Owner: arcfide
--

ALTER TABLE ONLY hastype
    ADD CONSTRAINT hastype_assignments_forkey FOREIGN KEY (assignment) REFERENCES assignments(name);


--
-- TOC entry 1914 (class 2606 OID 16873)
-- Dependencies: 150 1866 142
-- Name: hastype_categories_forkey; Type: FK CONSTRAINT; Schema: public; Owner: arcfide
--

ALTER TABLE ONLY hastype
    ADD CONSTRAINT hastype_categories_forkey FOREIGN KEY (category) REFERENCES categories(name);


--
-- TOC entry 1915 (class 2606 OID 16878)
-- Dependencies: 148 1879 152
-- Name: memberof_groups_forkey; Type: FK CONSTRAINT; Schema: public; Owner: arcfide
--

ALTER TABLE ONLY memberof
    ADD CONSTRAINT memberof_groups_forkey FOREIGN KEY (group_name) REFERENCES groups(name);


--
-- TOC entry 1916 (class 2606 OID 16883)
-- Dependencies: 152 155 1893
-- Name: memberof_submitters_forkey; Type: FK CONSTRAINT; Schema: public; Owner: arcfide
--

ALTER TABLE ONLY memberof
    ADD CONSTRAINT memberof_submitters_forkey FOREIGN KEY (student) REFERENCES submitters(networkid);


--
-- TOC entry 1917 (class 2606 OID 16888)
-- Dependencies: 154 1862 140
-- Name: submissions_assignments_forkey; Type: FK CONSTRAINT; Schema: public; Owner: arcfide
--

ALTER TABLE ONLY submissions
    ADD CONSTRAINT submissions_assignments_forkey FOREIGN KEY (submittedfor) REFERENCES assignments(name);


--
-- TOC entry 1918 (class 2606 OID 16893)
-- Dependencies: 154 1893 155
-- Name: submissions_submitters_forkey; Type: FK CONSTRAINT; Schema: public; Owner: arcfide
--

ALTER TABLE ONLY submissions
    ADD CONSTRAINT submissions_submitters_forkey FOREIGN KEY (owner) REFERENCES submitters(networkid);


--
-- TOC entry 1919 (class 2606 OID 16898)
-- Dependencies: 1879 156 148
-- Name: teaches_groups_forkey; Type: FK CONSTRAINT; Schema: public; Owner: arcfide
--

ALTER TABLE ONLY teaches
    ADD CONSTRAINT teaches_groups_forkey FOREIGN KEY (group_name) REFERENCES groups(name);


--
-- TOC entry 1920 (class 2606 OID 16903)
-- Dependencies: 151 156 1885
-- Name: teaches_instructors_forkey; Type: FK CONSTRAINT; Schema: public; Owner: arcfide
--

ALTER TABLE ONLY teaches
    ADD CONSTRAINT teaches_instructors_forkey FOREIGN KEY (teacher) REFERENCES instructors(networkid);


--
-- TOC entry 1921 (class 2606 OID 16908)
-- Dependencies: 140 1862 157
-- Name: validates_assignment_forkey; Type: FK CONSTRAINT; Schema: public; Owner: arcfide
--

ALTER TABLE ONLY validates
    ADD CONSTRAINT validates_assignment_forkey FOREIGN KEY (assignment) REFERENCES assignments(name);


--
-- TOC entry 1922 (class 2606 OID 16913)
-- Dependencies: 1899 158 157
-- Name: validates_validator_forkey; Type: FK CONSTRAINT; Schema: public; Owner: arcfide
--

ALTER TABLE ONLY validates
    ADD CONSTRAINT validates_validator_forkey FOREIGN KEY (validator) REFERENCES validators(name);


--
-- TOC entry 1927 (class 0 OID 0)
-- Dependencies: 3
-- Name: public; Type: ACL; Schema: -; Owner: postgres
--

REVOKE ALL ON SCHEMA public FROM PUBLIC;
REVOKE ALL ON SCHEMA public FROM postgres;
GRANT ALL ON SCHEMA public TO postgres;
GRANT ALL ON SCHEMA public TO PUBLIC;


-- Completed on 2013-02-18 18:59:05 EST

--
-- PostgreSQL database dump complete
--

