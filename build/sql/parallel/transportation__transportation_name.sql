DO $$ BEGIN RAISE NOTICE 'Processing layer transportation'; END$$;

DO $$ BEGIN
    PERFORM 'ne_10m_admin_0_countries'::regclass;
EXCEPTION
    WHEN undefined_table THEN
        RAISE EXCEPTION '%', SQLERRM
            USING DETAIL = 'this table or view is required for layer "transportation"';
END;
$$ LANGUAGE 'plpgsql';

-- Layer transportation - ./network_type.sql

DROP TRIGGER IF EXISTS trigger_store_transportation_route_member ON osm_route_member;
DROP TRIGGER IF EXISTS trigger_store_transportation_highway_linestring ON osm_highway_linestring;
DROP TRIGGER IF EXISTS trigger_flag_transportation_name ON transportation_name.network_changes;
DROP TRIGGER IF EXISTS trigger_refresh_network ON transportation_name.updates_network;

DROP TRIGGER IF EXISTS trigger_store_transportation_name_network ON osm_transportation_name_network;
DROP TRIGGER IF EXISTS trigger_flag_name ON transportation_name.name_changes;
DROP TRIGGER IF EXISTS trigger_refresh_name ON transportation_name.updates_name;

DO
$$
    BEGIN
        PERFORM 'route_network_type'::regtype;
    EXCEPTION
        WHEN undefined_object THEN
            CREATE TYPE route_network_type AS enum (
                'us-interstate', 'us-highway', 'us-state',
                'ca-transcanada', 'ca-provincial-arterial', 'ca-provincial',
                'gb-motorway', 'gb-trunk', 'gb-primary',
                'ie-motorway', 'ie-national', 'ie-regional'
                );
    END
$$;

-- Top-level national route networks that should display at the lowest zooms
CREATE OR REPLACE FUNCTION osm_national_network(network text) RETURNS boolean AS
$$
    SELECT network <> '' AND network IN (
        -- Canada
        'ca-transcanada', 'ca-provincial-arterial',
        -- United States
        'us-interstate');
$$ LANGUAGE sql IMMUTABLE
                PARALLEL SAFE;

DO
$$
    BEGIN
        BEGIN
            ALTER TABLE osm_route_member
                ADD COLUMN network_type route_network_type;
        EXCEPTION
            WHEN duplicate_column THEN RAISE NOTICE 'column network_type already exists in network_type.';
        END;
    END;
$$;

-- Layer transportation - ./class.sql

CREATE OR REPLACE FUNCTION brunnel(is_bridge bool, is_tunnel bool, is_ford bool) RETURNS text AS
$$
SELECT CASE
           WHEN is_bridge THEN 'bridge'
           WHEN is_tunnel THEN 'tunnel'
           WHEN is_ford THEN 'ford'
           END;
$$ LANGUAGE SQL IMMUTABLE
                STRICT
                PARALLEL SAFE;

-- The classes for highways are derived from the classes used in ClearTables
-- https://github.com/ClearTables/ClearTables/blob/master/transportation.lua
CREATE OR REPLACE FUNCTION highway_class(highway text, public_transport text, construction text) RETURNS text AS
$$
SELECT CASE
           WHEN "highway" IN ('motorway', 'motorway_link') THEN 'motorway'
           WHEN "highway" IN ('trunk', 'trunk_link') THEN 'trunk'
           WHEN "highway" IN ('primary', 'primary_link') THEN 'primary'
           WHEN "highway" IN ('secondary', 'secondary_link') THEN 'secondary'
           WHEN "highway" IN ('tertiary', 'tertiary_link') THEN 'tertiary'
           WHEN "highway" IN ('unclassified', 'residential', 'living_street', 'road') THEN 'minor'
           WHEN "highway" IN ('pedestrian', 'path', 'footway', 'cycleway', 'steps', 'bridleway', 'corridor')
               OR "public_transport" = 'platform'
               THEN 'path'
           WHEN "highway" = 'service' THEN 'service'
           WHEN "highway" = 'track' THEN 'track'
           WHEN "highway" = 'raceway' THEN 'raceway'
           WHEN "highway" = 'busway' THEN 'busway'
           WHEN "highway" = 'bus_guideway' THEN 'bus_guideway'
           WHEN "highway" = 'shipway' THEN 'ferry'
           WHEN "highway" = 'construction'
               AND "construction" IN ('motorway', 'motorway_link')
               THEN 'motorway_construction'
           WHEN "highway" = 'construction'
               AND "construction" IN ('trunk', 'trunk_link')
               THEN 'trunk_construction'
           WHEN "highway" = 'construction'
               AND "construction" IN ('primary', 'primary_link')
               THEN 'primary_construction'
           WHEN "highway" = 'construction'
               AND "construction" IN ('secondary', 'secondary_link')
               THEN 'secondary_construction'
           WHEN "highway" = 'construction'
               AND "construction" IN ('tertiary', 'tertiary_link')
               THEN 'tertiary_construction'
           WHEN "highway" = 'construction'
               AND "construction" IN ('', 'unclassified', 'residential', 'living_street', 'road')
               THEN 'minor_construction'
           WHEN "highway" = 'construction'
               AND ("construction" IN ('pedestrian', 'path', 'footway', 'cycleway', 'steps', 'bridleway', 'corridor') OR "public_transport" = 'platform')
               THEN 'path_construction'
           WHEN "highway" = 'construction'
               AND "construction" = 'service'
               THEN 'service_construction'
           WHEN "highway" = 'construction'
               AND "construction" = 'track'
               THEN 'track_construction'
           WHEN "highway" = 'construction'
               AND "construction" = 'raceway'
               THEN 'raceway_construction'
           END;
$$ LANGUAGE SQL IMMUTABLE
                PARALLEL SAFE;

-- The classes for railways are derived from the classes used in ClearTables
-- https://github.com/ClearTables/ClearTables/blob/master/transportation.lua
CREATE OR REPLACE FUNCTION railway_class(railway text) RETURNS text AS
$$
SELECT CASE
           WHEN railway IN ('rail', 'narrow_gauge', 'preserved', 'funicular') THEN 'rail'
           WHEN railway IN ('subway', 'light_rail', 'monorail', 'tram') THEN 'transit'
           END;
$$ LANGUAGE SQL IMMUTABLE
                STRICT
                PARALLEL SAFE;

-- Limit service to only the most important values to ensure
-- we always know the values of service
CREATE OR REPLACE FUNCTION service_value(service text) RETURNS text AS
$$
SELECT CASE
           WHEN service IN ('spur', 'yard', 'siding', 'crossover', 'driveway', 'alley', 'parking_aisle') THEN service
           END;
$$ LANGUAGE SQL IMMUTABLE
                STRICT
                PARALLEL SAFE;

-- Limit surface to only the most important values to ensure
-- we always know the values of surface
CREATE OR REPLACE FUNCTION surface_value(surface text) RETURNS text AS
$$
SELECT CASE
           WHEN surface IN ('paved', 'asphalt', 'cobblestone', 'concrete', 'concrete:lanes', 'concrete:plates', 'metal',
                            'paving_stones', 'sett', 'unhewn_cobblestone', 'wood', 'grade1') THEN 'paved'
           WHEN surface IN ('unpaved', 'compacted', 'dirt', 'earth', 'fine_gravel', 'grass', 'grass_paver', 'gravel',
                            'gravel_turf', 'ground', 'ice', 'mud', 'pebblestone', 'salt', 'sand', 'snow', 'woodchips')
               THEN 'unpaved'
           END;
$$ LANGUAGE SQL IMMUTABLE
                STRICT
                PARALLEL SAFE;

-- Determine which transportation features are shown at zoom 12
CREATE OR REPLACE FUNCTION transportation_filter_z12(highway text, construction text) RETURNS boolean AS
$$
SELECT CASE
           WHEN highway IN ('unclassified', 'residential') THEN TRUE
           WHEN highway_class(highway, '', construction) IN
               (
                'motorway', 'trunk', 'primary', 'secondary', 'tertiary', 'raceway',
                'motorway_construction', 'trunk_construction', 'primary_construction',
                'secondary_construction', 'tertiary_construction', 'raceway_construction',
                'busway', 'bus_guideway'
               ) THEN TRUE --includes ramps
           ELSE FALSE
       END
$$ LANGUAGE SQL IMMUTABLE
                STRICT
                PARALLEL SAFE;

-- Determine which transportation features are shown at zoom 13
-- Assumes that piers have already been excluded
CREATE OR REPLACE FUNCTION transportation_filter_z13(highway text,
                                                     public_transport text,
                                                     construction text,
                                                     service text) RETURNS boolean AS
$$
SELECT CASE
           WHEN transportation_filter_z12(highway, construction) THEN TRUE
           WHEN highway = 'service' OR construction = 'service' THEN service NOT IN ('driveway', 'parking_aisle')
           WHEN highway_class(highway, public_transport, construction) IN ('minor', 'minor_construction') THEN TRUE
           ELSE FALSE
       END
$$ LANGUAGE SQL IMMUTABLE
                STRICT
                PARALLEL SAFE;

-- Layer transportation - ./highway_name.sql

CREATE OR REPLACE FUNCTION transportation_name_tags(geometry geometry, tags hstore, name text, name_en text, name_de text) RETURNS hstore AS
$$
SELECT hstore(string_agg(nullif(slice_language_tags(tags ||
                     hstore(ARRAY [
                       'name',    CASE WHEN length(name) > 15    THEN osml10n_street_abbrev_all(name)   ELSE NULLIF(name, '') END,
                       'name:en', CASE WHEN length(name_en) > 15 THEN osml10n_street_abbrev_en(name_en) ELSE NULLIF(name_en, '') END,
                       'name:de', CASE WHEN length(name_de) > 15 THEN osml10n_street_abbrev_de(name_de) ELSE NULLIF(name_de, '') END
                     ]))::text,
                     ''), ','));
$$ LANGUAGE SQL IMMUTABLE
                PARALLEL SAFE;

-- Layer transportation - ./update_route_member.sql

-- Create bounding windows for country-specific processing

-- etldoc: ne_10m_admin_0_countries ->  ne_10m_admin_0_gb_buffer
CREATE TABLE IF NOT EXISTS ne_10m_admin_0_gb_buffer AS
SELECT ST_Buffer(geometry, 10000)
FROM ne_10m_admin_0_countries
WHERE iso_a2 = 'GB';

-- etldoc: ne_10m_admin_0_countries ->  ne_10m_admin_0_ie_buffer
CREATE TABLE IF NOT EXISTS ne_10m_admin_0_ie_buffer AS
SELECT ST_Buffer(geometry, 10000)
FROM ne_10m_admin_0_countries
WHERE iso_a2 = 'IE';

-- Assign pseudo-networks based highway classification
-- etldoc:  osm_highway_linestring ->  gbr_route_members_view
-- etldoc:  ne_10m_admin_0_gb_buffer ->  gbr_route_members_view
CREATE OR REPLACE VIEW gbr_route_members_view AS
SELECT 0,
       osm_id,
       substring(ref FROM E'^[ABM][0-9ABM()]+'),
       -- See https://wiki.openstreetmap.org/wiki/Roads_in_the_United_Kingdom
       CASE WHEN highway = 'motorway' THEN 'omt-gb-motorway'
            WHEN highway = 'trunk' THEN 'omt-gb-trunk' 
            WHEN highway IN ('primary','secondary') THEN 'omt-gb-primary' END AS network
FROM osm_highway_linestring
WHERE length(ref) > 1
  AND ST_Intersects(geometry, (SELECT * FROM ne_10m_admin_0_gb_buffer))
  AND highway IN ('motorway', 'trunk', 'primary', 'secondary')
;

-- etldoc:  osm_highway_linestring ->  ire_route_members_view
-- etldoc:  ne_10m_admin_0_ie_buffer ->  ire_route_members_view
CREATE OR REPLACE VIEW ire_route_members_view AS
SELECT 0,
       osm_id,
       substring(ref FROM E'^[MNRL][0-9]+'),
       -- See https://wiki.openstreetmap.org/wiki/Ireland/Roads
       CASE WHEN highway = 'motorway' THEN 'omt-ie-motorway'
            WHEN highway IN ('trunk','primary') THEN 'omt-ie-national' 
            ELSE 'omt-ie-regional' END AS network
FROM osm_highway_linestring
WHERE length(ref) > 1
  AND ST_Intersects(geometry, (SELECT * FROM ne_10m_admin_0_ie_buffer))
  AND highway IN ('motorway', 'trunk', 'primary', 'secondary', 'unclassified')
;

-- Create GBR/IRE relations (so we can use it in the same way as other relations)
-- etldoc:  osm_route_member ->  osm_route_member
DELETE
FROM osm_route_member
WHERE network IN ('omt-gb-motorway', 'omt-gb-trunk', 'omt-gb-primary',
                  'omt-ie-motorway', 'omt-ie-national', 'omt-ie-national');

-- etldoc:  gbr_route_members_view ->  osm_route_member
INSERT INTO osm_route_member (osm_id, member, ref, network)
SELECT *
FROM gbr_route_members_view;

-- etldoc:  ire_route_members_view ->  osm_route_member
INSERT INTO osm_route_member (osm_id, member, ref, network)
SELECT *
FROM ire_route_members_view;

CREATE OR REPLACE FUNCTION osm_route_member_network_type(network text, ref text) RETURNS route_network_type AS
$$
SELECT CASE
           WHEN network = 'US:I' THEN 'us-interstate'::route_network_type
           WHEN network = 'US:US' THEN 'us-highway'::route_network_type
           WHEN network LIKE 'US:__' THEN 'us-state'::route_network_type
           -- https://en.wikipedia.org/wiki/Trans-Canada_Highway
           WHEN network LIKE 'CA:transcanada%' THEN 'ca-transcanada'::route_network_type
           WHEN network = 'CA:QC:A' THEN 'ca-provincial-arterial'::route_network_type
           WHEN network = 'CA:ON:primary' THEN
               CASE
                   WHEN ref LIKE '4__' THEN 'ca-provincial-arterial'::route_network_type
                   WHEN ref = 'QEW' THEN 'ca-provincial-arterial'::route_network_type
                   ELSE 'ca-provincial-arterial'::route_network_type
               END
           WHEN network = 'CA:MB:PTH' AND ref = '75' THEN 'ca-provincial-arterial'::route_network_type
           WHEN network = 'CA:AB:primary' AND ref IN ('2','3','4') THEN 'ca-provincial-arterial'::route_network_type
           WHEN network = 'CA:BC' AND ref IN ('3','5','99') THEN 'ca-provincial-arterial'::route_network_type
           WHEN network LIKE 'CA:__' OR network LIKE 'CA:__:%' THEN 'ca-provincial'::route_network_type
           WHEN network = 'omt-gb-motorway' THEN 'gb-motorway'::route_network_type
           WHEN network = 'omt-gb-trunk' THEN 'gb-trunk'::route_network_type
           WHEN network = 'omt-gb-primary' THEN 'gb-primary'::route_network_type
           WHEN network = 'omt-ie-motorway' THEN 'ie-motorway'::route_network_type
           WHEN network = 'omt-ie-national' THEN 'ie-national'::route_network_type
           WHEN network = 'omt-ie-regional' THEN 'ie-regional'::route_network_type
            END;
$$ LANGUAGE sql IMMUTABLE
                PARALLEL SAFE;

-- etldoc:  osm_route_member ->  osm_route_member
-- see http://wiki.openstreetmap.org/wiki/Relation:route#Road_routes
UPDATE osm_route_member
SET network_type = osm_route_member_network_type(network, ref)
WHERE network != ''
  AND network_type IS DISTINCT FROM osm_route_member_network_type(network, ref)
;

CREATE OR REPLACE FUNCTION update_osm_route_member() RETURNS void AS
$$
BEGIN
    DELETE
    FROM osm_route_member AS r
        USING
            transportation_name.network_changes AS c
    WHERE network IN ('omt-gb-motorway', 'omt-gb-trunk', 'omt-gb-primary',
                      'omt-ie-motorway', 'omt-ie-national', 'omt-ie-regional')
      AND r.osm_id = c.osm_id;

    INSERT INTO osm_route_member (osm_id, member, ref, network)
    SELECT r.*
    FROM gbr_route_members_view AS r
             JOIN transportation_name.network_changes AS c ON
        r.osm_id = c.osm_id;

    INSERT INTO osm_route_member (osm_id, member, ref, network)
    SELECT r.*
    FROM ire_route_members_view AS r
             JOIN transportation_name.network_changes AS c ON
        r.osm_id = c.osm_id;

    INSERT INTO osm_route_member (id, osm_id, network_type)
    SELECT
      id,
      osm_id,
      osm_route_member_network_type(network, ref) AS network_type
    FROM osm_route_member rm
    WHERE rm.member IN
      (SELECT DISTINCT osm_id FROM transportation_name.network_changes)
    ON CONFLICT (id, osm_id) DO UPDATE SET network_type = EXCLUDED.network_type;
    REFRESH MATERIALIZED VIEW transportation_route_member_coalesced;
END;
$$ LANGUAGE plpgsql;

CREATE INDEX IF NOT EXISTS osm_route_member_osm_id_idx ON osm_route_member ("osm_id");

-- etldoc: osm_route_member ->  transportation_route_member_coalesced
DROP MATERIALIZED VIEW IF EXISTS transportation_route_member_coalesced CASCADE;
CREATE MATERIALIZED VIEW transportation_route_member_coalesced AS
SELECT
  member, 
  network_type,
  network,
  ref,
  DENSE_RANK() over (PARTITION BY member ORDER BY network_type, network, LENGTH(ref), ref) AS concurrency_index,
  rank
FROM (
  SELECT DISTINCT
    member,
    network_type,
    network,
    ref,
    CASE
         WHEN network IN ('iwn', 'nwn', 'rwn') THEN 1
         WHEN network = 'lwn' THEN 2
         WHEN osmc_symbol || colour <> '' THEN 2
    END AS rank
  FROM osm_route_member
) osm_route_member_filtered
GROUP BY member, network_type, network, ref, rank;

CREATE INDEX IF NOT EXISTS transportation_route_member_member_idx ON transportation_route_member_coalesced ("member");
CREATE INDEX IF NOT EXISTS osm_highway_linestring_osm_id_idx ON osm_highway_linestring ("osm_id");

-- etldoc:  osm_route_member ->  osm_highway_linestring
UPDATE osm_highway_linestring hl
  SET network = rm.network_type
  FROM transportation_route_member_coalesced rm
  WHERE hl.osm_id=rm.member AND rm.concurrency_index=1;

-- etldoc:  osm_route_member ->  osm_highway_linestring_gen_z11
UPDATE osm_highway_linestring_gen_z11 hl
  SET network = rm.network_type
  FROM transportation_route_member_coalesced rm
  WHERE hl.osm_id=rm.member AND rm.concurrency_index=1;

-- Layer transportation - ./update_transportation_merge.sql

DROP TRIGGER IF EXISTS trigger_osm_transportation_merge_linestring_gen_z8 ON osm_transportation_merge_linestring_gen_z8;
DROP TRIGGER IF EXISTS trigger_store_transportation_highway_linestring_gen_z9 ON osm_transportation_merge_linestring_gen_z9;
DROP TRIGGER IF EXISTS trigger_flag_transportation_z9 ON osm_transportation_merge_linestring_gen_z9;
DROP TRIGGER IF EXISTS trigger_refresh_z8 ON transportation.updates_z9;
DROP TRIGGER IF EXISTS trigger_osm_transportation_merge_linestring_gen_z11 ON osm_transportation_merge_linestring_gen_z11;
DROP TRIGGER IF EXISTS trigger_store_transportation_highway_linestring_gen_z11 ON osm_highway_linestring_gen_z11;
DROP TRIGGER IF EXISTS trigger_flag_transportation_z11 ON osm_highway_linestring_gen_z11;
DROP TRIGGER IF EXISTS trigger_refresh_z11 ON transportation.updates_z11;

-- Instead of using relations to find out the road names we
-- stitch together the touching ways with the same name
-- to allow for nice label rendering
-- Because this works well for roads that do not have relations as well

-- etldoc: osm_highway_linestring ->  osm_transportation_name_network
-- etldoc: transportation_route_member_coalesced ->  osm_transportation_name_network
CREATE TABLE IF NOT EXISTS osm_transportation_name_network AS
SELECT
    geometry,
    osm_id,
    tags || get_basic_names(tags, geometry) AS tags,
    ref,
    highway,
    subclass,
    brunnel,
    "level",
    sac_scale,
    layer,
    indoor,
    network_type,
    route_1, route_2, route_3, route_4, route_5, route_6,
    z_order,
    route_rank
FROM (
    SELECT DISTINCT ON (hl.osm_id)
        hl.geometry,
        hl.osm_id,
        transportation_name_tags(hl.geometry, hl.tags, hl.name, hl.name_en, hl.name_de) AS tags,
        rm1.network_type,
        CASE
            WHEN rm1.network_type IS NOT NULL AND rm1.ref::text <> ''
                THEN rm1.ref::text
            ELSE NULLIF(hl.ref, '')
            END AS ref,
        hl.highway,
        NULLIF(hl.construction, '') AS subclass,
        brunnel(hl.is_bridge, hl.is_tunnel, hl.is_ford) AS brunnel,
        sac_scale,
        CASE WHEN highway IN ('footway', 'steps') THEN layer END AS layer,
        CASE WHEN highway IN ('footway', 'steps') THEN level END AS level,
        CASE WHEN highway IN ('footway', 'steps') THEN indoor END AS indoor,
        NULLIF(rm1.network, '') || '=' || COALESCE(rm1.ref, '') AS route_1,
        NULLIF(rm2.network, '') || '=' || COALESCE(rm2.ref, '') AS route_2,
        NULLIF(rm3.network, '') || '=' || COALESCE(rm3.ref, '') AS route_3,
        NULLIF(rm4.network, '') || '=' || COALESCE(rm4.ref, '') AS route_4,
        NULLIF(rm5.network, '') || '=' || COALESCE(rm5.ref, '') AS route_5,
        NULLIF(rm6.network, '') || '=' || COALESCE(rm6.ref, '') AS route_6,
        hl.z_order,
        LEAST(rm1.rank, rm2.rank, rm3.rank, rm4.rank, rm5.rank, rm6.rank) AS route_rank
    FROM osm_highway_linestring hl
            LEFT OUTER JOIN transportation_route_member_coalesced rm1 ON rm1.member = hl.osm_id AND rm1.concurrency_index=1
            LEFT OUTER JOIN transportation_route_member_coalesced rm2 ON rm2.member = hl.osm_id AND rm2.concurrency_index=2
            LEFT OUTER JOIN transportation_route_member_coalesced rm3 ON rm3.member = hl.osm_id AND rm3.concurrency_index=3
            LEFT OUTER JOIN transportation_route_member_coalesced rm4 ON rm4.member = hl.osm_id AND rm4.concurrency_index=4
            LEFT OUTER JOIN transportation_route_member_coalesced rm5 ON rm5.member = hl.osm_id AND rm5.concurrency_index=5
            LEFT OUTER JOIN transportation_route_member_coalesced rm6 ON rm6.member = hl.osm_id AND rm6.concurrency_index=6
    WHERE (hl.name <> '' OR hl.ref <> '' OR rm1.ref <> '' OR rm1.network <> '')
      AND hl.highway <> ''
) AS t;
CREATE UNIQUE INDEX IF NOT EXISTS osm_transportation_name_network_osm_id_idx ON osm_transportation_name_network (osm_id);
CREATE INDEX IF NOT EXISTS osm_transportation_name_network_name_ref_idx ON osm_transportation_name_network (coalesce(tags->'name', ''), coalesce(ref, ''));
CREATE INDEX IF NOT EXISTS osm_transportation_name_network_geometry_idx ON osm_transportation_name_network USING gist (geometry);

-- Improve performance of the sql in transportation/update_route_member.sql
CREATE INDEX IF NOT EXISTS osm_highway_linestring_highway_partial_idx
    ON osm_highway_linestring (highway)
    WHERE highway IN ('motorway', 'trunk');


-- etldoc: osm_highway_linestring_gen_z11 ->  osm_transportation_merge_linestring_gen_z11
CREATE TABLE IF NOT EXISTS osm_transportation_merge_linestring_gen_z11(
    geometry geometry,
    id SERIAL PRIMARY KEY,
    osm_id bigint,
    highway character varying,
    network character varying,
    construction character varying,
    is_bridge boolean,
    is_tunnel boolean,
    is_ford boolean,
    expressway boolean,
    z_order integer,
    bicycle character varying,
    foot character varying,
    horse character varying,
    mtb_scale character varying,
    sac_scale character varying,
    access text,
    toll boolean,
    layer integer
);

INSERT INTO osm_transportation_merge_linestring_gen_z11(geometry, osm_id, highway, network, construction, is_bridge, is_tunnel, is_ford, expressway, z_order, bicycle, foot, horse, mtb_scale, sac_scale, access, toll, layer)
SELECT (ST_Dump(ST_LineMerge(ST_Collect(geometry)))).geom AS geometry,
       NULL::bigint AS osm_id,
       highway,
       network,
       construction,
       is_bridge,
       is_tunnel,
       is_ford,
       expressway,
       min(z_order) as z_order,
       bicycle,
       foot,
       horse,
       mtb_scale,
       sac_scale,
       CASE
           WHEN access IN ('private', 'no') THEN 'no'
           ELSE NULL::text END AS access,
       toll,
       layer
FROM osm_highway_linestring_gen_z11
-- mapping.yaml pre-filter: motorway/trunk/primary/secondary/tertiary, with _link variants, construction, ST_IsValid()
GROUP BY highway, network, construction, is_bridge, is_tunnel, is_ford, expressway, bicycle, foot, horse, mtb_scale, sac_scale, access, toll, layer
;
CREATE INDEX IF NOT EXISTS osm_transportation_merge_linestring_gen_z11_geometry_idx
    ON osm_transportation_merge_linestring_gen_z11 USING gist (geometry);


CREATE TABLE IF NOT EXISTS osm_transportation_merge_linestring_gen_z10
    (LIKE osm_transportation_merge_linestring_gen_z11);

CREATE TABLE IF NOT EXISTS osm_transportation_merge_linestring_gen_z9
    (LIKE osm_transportation_merge_linestring_gen_z10);


CREATE OR REPLACE FUNCTION insert_transportation_merge_linestring_gen_z10(update_id bigint) RETURNS void AS
$$
BEGIN
    DELETE FROM osm_transportation_merge_linestring_gen_z10
    WHERE update_id IS NULL OR id = update_id;

    -- etldoc: osm_transportation_merge_linestring_gen_z11 -> osm_transportation_merge_linestring_gen_z10
    INSERT INTO osm_transportation_merge_linestring_gen_z10
    SELECT ST_Simplify(geometry, ZRes(12)) AS geometry,
        id,
        osm_id,
        highway,
        network,
        construction,
        is_bridge,
        is_tunnel,
        is_ford,
        expressway,
        z_order,
        bicycle,
        foot,
        horse,
        mtb_scale,
        sac_scale,
        access,
        toll,
        layer
    FROM osm_transportation_merge_linestring_gen_z11
    WHERE (update_id IS NULL OR id = update_id)
        AND highway NOT IN ('tertiary', 'tertiary_link', 'busway', 'bus_guideway')
        AND construction NOT IN ('tertiary', 'tertiary_link', 'busway', 'bus_guideway')
    ;

    DELETE FROM osm_transportation_merge_linestring_gen_z9
    WHERE update_id IS NULL OR id = update_id;

    -- etldoc: osm_transportation_merge_linestring_gen_z10 -> osm_transportation_merge_linestring_gen_z9
    INSERT INTO osm_transportation_merge_linestring_gen_z9
    SELECT ST_Simplify(geometry, ZRes(11)) AS geometry,
        id,
        osm_id,
        highway,
        network,
        construction,
        is_bridge,
        is_tunnel,
        is_ford,
        expressway,
        z_order,
        bicycle,
        foot,
        horse,
        mtb_scale,
        sac_scale,
        access,
        toll,
        layer
    FROM osm_transportation_merge_linestring_gen_z10
    WHERE (update_id IS NULL OR id = update_id)
    ;
END;
$$ LANGUAGE plpgsql;

SELECT insert_transportation_merge_linestring_gen_z10(NULL);

CREATE INDEX IF NOT EXISTS osm_transportation_merge_linestring_gen_z10_geometry_idx
    ON osm_transportation_merge_linestring_gen_z10 USING gist (geometry);
CREATE UNIQUE INDEX IF NOT EXISTS osm_transportation_merge_linestring_gen_z10_id_idx
    ON osm_transportation_merge_linestring_gen_z10(id);

CREATE INDEX IF NOT EXISTS osm_transportation_merge_linestring_gen_z9_geometry_idx
    ON osm_transportation_merge_linestring_gen_z9 USING gist (geometry);
CREATE UNIQUE INDEX IF NOT EXISTS osm_transportation_merge_linestring_gen_z9_id_idx
    ON osm_transportation_merge_linestring_gen_z9(id);


-- etldoc: osm_transportation_merge_linestring_gen_z9 -> osm_transportation_merge_linestring_gen_z8
CREATE TABLE IF NOT EXISTS osm_transportation_merge_linestring_gen_z8(
    geometry geometry,
    id SERIAL PRIMARY KEY,
    osm_id bigint,
    highway character varying,
    network character varying,
    construction character varying,
    is_bridge boolean,
    is_tunnel boolean,
    is_ford boolean,
    expressway boolean,
    z_order integer
);

INSERT INTO osm_transportation_merge_linestring_gen_z8(geometry, osm_id, highway, network, construction, is_bridge, is_tunnel, is_ford, expressway, z_order)
SELECT ST_Simplify(ST_LineMerge(ST_Collect(geometry)), ZRes(10)) AS geometry,
       NULL::bigint AS osm_id,
       highway,
       network,
       construction,
       is_bridge,
       is_tunnel,
       is_ford,
       expressway,
       min(z_order) as z_order
FROM osm_transportation_merge_linestring_gen_z9
WHERE (highway IN ('motorway', 'trunk', 'primary') OR
       construction IN ('motorway', 'trunk', 'primary'))
       AND ST_IsValid(geometry)
       AND access IS NULL
GROUP BY highway, network, construction, is_bridge, is_tunnel, is_ford, expressway
;
CREATE INDEX IF NOT EXISTS osm_transportation_merge_linestring_gen_z8_geometry_idx
    ON osm_transportation_merge_linestring_gen_z8 USING gist (geometry);

CREATE TABLE IF NOT EXISTS osm_transportation_merge_linestring_gen_z7
    (LIKE osm_transportation_merge_linestring_gen_z8);

CREATE TABLE IF NOT EXISTS osm_transportation_merge_linestring_gen_z6
    (LIKE osm_transportation_merge_linestring_gen_z7);

CREATE TABLE IF NOT EXISTS osm_transportation_merge_linestring_gen_z5
    (LIKE osm_transportation_merge_linestring_gen_z6);

CREATE TABLE IF NOT EXISTS osm_transportation_merge_linestring_gen_z4
    (LIKE osm_transportation_merge_linestring_gen_z5);


CREATE OR REPLACE FUNCTION insert_transportation_merge_linestring_gen_z7(update_id bigint) RETURNS void AS
$$
BEGIN
    DELETE FROM osm_transportation_merge_linestring_gen_z7
    WHERE update_id IS NULL OR id = update_id;

    -- etldoc: osm_transportation_merge_linestring_gen_z8 -> osm_transportation_merge_linestring_gen_z7
    INSERT INTO osm_transportation_merge_linestring_gen_z7
    SELECT ST_Simplify(geometry, ZRes(9)) AS geometry,
        id,
        osm_id,
        highway,
        network,
        construction,
        is_bridge,
        is_tunnel,
        is_ford,
        expressway,
        z_order
    FROM osm_transportation_merge_linestring_gen_z8
        -- Current view: motorway/trunk/primary
    WHERE
        (update_id IS NULL OR id = update_id) AND
        ST_Length(geometry) > 50;

    DELETE FROM osm_transportation_merge_linestring_gen_z6
    WHERE update_id IS NULL OR id = update_id;

    -- etldoc: osm_transportation_merge_linestring_gen_z7 -> osm_transportation_merge_linestring_gen_z6
    INSERT INTO osm_transportation_merge_linestring_gen_z6
    SELECT ST_Simplify(geometry, ZRes(8)) AS geometry,
        id,
        osm_id,
        highway,
        network,
        construction,
        is_bridge,
        is_tunnel,
        is_ford,
        expressway,
        z_order
    FROM osm_transportation_merge_linestring_gen_z7
    WHERE
        (update_id IS NULL OR id = update_id) AND
        (highway IN ('motorway', 'trunk') OR construction IN ('motorway', 'trunk')) AND
        ST_Length(geometry) > 100;

    DELETE FROM osm_transportation_merge_linestring_gen_z5
    WHERE update_id IS NULL OR id = update_id;

    -- etldoc: osm_transportation_merge_linestring_gen_z6 -> osm_transportation_merge_linestring_gen_z5
    INSERT INTO osm_transportation_merge_linestring_gen_z5
    SELECT ST_Simplify(geometry, ZRes(7)) AS geometry,
        id,
        osm_id,
        highway,
        network,
        construction,
        is_bridge,
        is_tunnel,
        is_ford,
        expressway,
        z_order
    FROM osm_transportation_merge_linestring_gen_z6
    WHERE
        (update_id IS NULL OR id = update_id) AND
        -- Current view: all motorways and trunks of national-importance
        (highway = 'motorway'
            OR construction = 'motorway'
            -- Allow trunk roads that are part of a nation's most important route network to show at z4
            OR (highway = 'trunk' AND osm_national_network(network))
        ) AND
        ST_Length(geometry) > 500;

    DELETE FROM osm_transportation_merge_linestring_gen_z4
    WHERE update_id IS NULL OR id = update_id;

    -- etldoc: osm_transportation_merge_linestring_gen_z5 -> osm_transportation_merge_linestring_gen_z4
    INSERT INTO osm_transportation_merge_linestring_gen_z4
    SELECT ST_Simplify(geometry, ZRes(6)) AS geometry,
        id,
        osm_id,
        highway,
        network,
        construction,
        is_bridge,
        is_tunnel,
        is_ford,
        expressway,
        z_order
    FROM osm_transportation_merge_linestring_gen_z5
    WHERE
        (update_id IS NULL OR id = update_id) AND
        osm_national_network(network) AND
        -- Current view: national-importance motorways and trunks
        ST_Length(geometry) > 1000;
END;
$$ LANGUAGE plpgsql;

SELECT insert_transportation_merge_linestring_gen_z7(NULL);

CREATE INDEX IF NOT EXISTS osm_transportation_merge_linestring_gen_z7_geometry_idx
    ON osm_transportation_merge_linestring_gen_z7 USING gist (geometry);
CREATE UNIQUE INDEX IF NOT EXISTS osm_transportation_merge_linestring_gen_z7_id_idx
    ON osm_transportation_merge_linestring_gen_z7(id);

CREATE INDEX IF NOT EXISTS osm_transportation_merge_linestring_gen_z6_geometry_idx
    ON osm_transportation_merge_linestring_gen_z6 USING gist (geometry);
CREATE UNIQUE INDEX IF NOT EXISTS osm_transportation_merge_linestring_gen_z6_id_idx
    ON osm_transportation_merge_linestring_gen_z6(id);

CREATE INDEX IF NOT EXISTS osm_transportation_merge_linestring_gen_z5_geometry_idx
    ON osm_transportation_merge_linestring_gen_z5 USING gist (geometry);
CREATE UNIQUE INDEX IF NOT EXISTS osm_transportation_merge_linestring_gen_z5_id_idx
    ON osm_transportation_merge_linestring_gen_z5(id);

CREATE INDEX IF NOT EXISTS osm_transportation_merge_linestring_gen_z4_geometry_idx
    ON osm_transportation_merge_linestring_gen_z4 USING gist (geometry);
CREATE UNIQUE INDEX IF NOT EXISTS osm_transportation_merge_linestring_gen_z4_id_idx
    ON osm_transportation_merge_linestring_gen_z4(id);


-- Handle updates on
-- osm_highway_linestring_gen_z11 -> osm_transportation_merge_linestring_gen_z11

CREATE SCHEMA IF NOT EXISTS transportation;

CREATE TABLE IF NOT EXISTS transportation.changes_z11
(
    id serial PRIMARY KEY,
    is_old boolean,
    geometry geometry,
    osm_id bigint,
    highway character varying,
    network character varying,
    construction character varying,
    is_bridge boolean,
    is_tunnel boolean,
    is_ford boolean,
    expressway boolean,
    z_order integer,
    bicycle character varying,
    foot character varying,
    horse character varying,
    mtb_scale character varying,
    sac_scale character varying,
    access character varying,
    toll boolean,
    layer integer
);

CREATE OR REPLACE FUNCTION transportation.store_z11() RETURNS trigger AS
$$
BEGIN
    IF (tg_op = 'DELETE' OR tg_op = 'UPDATE') THEN
        INSERT INTO transportation.changes_z11(is_old, geometry, osm_id, highway, network, construction, is_bridge, is_tunnel, is_ford, expressway, z_order, bicycle, foot, horse, mtb_scale, sac_scale, access, toll, layer)
        VALUES (true, old.geometry, old.osm_id, old.highway, old.network, old.construction, old.is_bridge, old.is_tunnel, old.is_ford, old.expressway, old.z_order, old.bicycle, old.foot, old.horse, old.mtb_scale, old.sac_scale,
            CASE
                WHEN old.access IN ('private', 'no') THEN 'no'
                ELSE NULL::text END,
            old.toll, old.layer);
    END IF;
    IF (tg_op = 'UPDATE' OR tg_op = 'INSERT') THEN
        INSERT INTO transportation.changes_z11(is_old, geometry, osm_id, highway, network, construction, is_bridge, is_tunnel, is_ford, expressway, z_order, bicycle, foot, horse, mtb_scale, sac_scale, access, toll, layer)
        VALUES (false, new.geometry, new.osm_id, new.highway, new.network, new.construction, new.is_bridge, new.is_tunnel, new.is_ford, new.expressway, new.z_order, new.bicycle, new.foot, new.horse, new.mtb_scale, new.sac_scale,
            CASE
                WHEN new.access IN ('private', 'no') THEN 'no'
                ELSE NULL::text END,
            new.toll, new.layer);
    END IF;
    RETURN NULL;
END;
$$ LANGUAGE plpgsql;

CREATE TABLE IF NOT EXISTS transportation.updates_z11
(
    id serial PRIMARY KEY,
    t text,
    UNIQUE (t)
);
CREATE OR REPLACE FUNCTION transportation.flag_z11() RETURNS trigger AS
$$
BEGIN
    INSERT INTO transportation.updates_z11(t) VALUES ('y') ON CONFLICT(t) DO NOTHING;
    RETURN NULL;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION transportation.refresh_z11() RETURNS trigger AS
$$
DECLARE
    t TIMESTAMP WITH TIME ZONE := clock_timestamp();
BEGIN
    RAISE LOG 'Refresh transportation z11';

    -- Compact the change history to keep only the first and last version
    CREATE TEMP TABLE changes_compact AS
    SELECT
        *
    FROM ((
        SELECT DISTINCT ON (osm_id) *
        FROM transportation.changes_z11
        WHERE is_old
        ORDER BY osm_id,
                 id ASC
    ) UNION ALL (
        SELECT DISTINCT ON (osm_id) *
        FROM transportation.changes_z11
        WHERE NOT is_old
        ORDER BY osm_id,
                 id DESC
    )) AS t;

    -- Collect all original existing ways from impacted mmerge
    CREATE TEMP TABLE osm_highway_linestring_original AS
    SELECT DISTINCT ON (h.osm_id)
        NULL::integer AS id,
        NULL::boolean AS is_old,
        h.geometry,
        h.osm_id,
        h.highway,
        h.network,
        h.construction,
        h.is_bridge,
        h.is_tunnel,
        h.is_ford,
        h.expressway,
        h.z_order,
        h.bicycle,
        h.foot,
        h.horse,
        h.mtb_scale,
        h.sac_scale,
        h.access,
        h.toll,
        h.layer
    FROM
        changes_compact AS c
        JOIN osm_transportation_merge_linestring_gen_z11 AS m ON
             m.geometry && c.geometry
             AND m.highway IS NOT DISTINCT FROM c.highway
             AND m.network IS NOT DISTINCT FROM c.network
             AND m.construction IS NOT DISTINCT FROM c.construction
             AND m.is_bridge IS NOT DISTINCT FROM c.is_bridge
             AND m.is_tunnel IS NOT DISTINCT FROM c.is_tunnel
             AND m.is_ford IS NOT DISTINCT FROM c.is_ford
             AND m.expressway IS NOT DISTINCT FROM c.expressway
             AND m.bicycle IS NOT DISTINCT FROM c.bicycle
             AND m.foot IS NOT DISTINCT FROM c.foot
             AND m.horse IS NOT DISTINCT FROM c.horse
             AND m.mtb_scale IS NOT DISTINCT FROM c.mtb_scale
             AND m.sac_scale IS NOT DISTINCT FROM c.sac_scale
             AND m.access IS NOT DISTINCT FROM c.access
             AND m.toll IS NOT DISTINCT FROM c.toll
             AND m.layer IS NOT DISTINCT FROM c.layer
        JOIN osm_highway_linestring_gen_z11 AS h ON
             h.geometry && c.geometry
             AND h.osm_id NOT IN (SELECT osm_id FROM changes_compact)
             AND ST_Contains(m.geometry, h.geometry)
             AND h.highway IS NOT DISTINCT FROM m.highway
             AND h.network IS NOT DISTINCT FROM m.network
             AND h.construction IS NOT DISTINCT FROM m.construction
             AND h.is_bridge IS NOT DISTINCT FROM m.is_bridge
             AND h.is_tunnel IS NOT DISTINCT FROM m.is_tunnel
             AND h.is_ford IS NOT DISTINCT FROM m.is_ford
             AND h.expressway IS NOT DISTINCT FROM m.expressway
             AND h.bicycle IS NOT DISTINCT FROM m.bicycle
             AND h.foot IS NOT DISTINCT FROM m.foot
             AND h.horse IS NOT DISTINCT FROM m.horse
             AND h.mtb_scale IS NOT DISTINCT FROM m.mtb_scale
             AND h.sac_scale IS NOT DISTINCT FROM m.sac_scale
             AND CASE
                WHEN h.access IN ('private', 'no') THEN 'no'
                ELSE NULL::text END IS NOT DISTINCT FROM m.access
             AND h.toll IS NOT DISTINCT FROM m.toll
             AND h.layer IS NOT DISTINCT FROM m.layer
    ORDER BY
        h.osm_id
    ;

    DELETE
    FROM osm_transportation_merge_linestring_gen_z11 AS m
        USING changes_compact AS c
    WHERE
        m.geometry && c.geometry
        AND m.highway IS NOT DISTINCT FROM c.highway
        AND m.network IS NOT DISTINCT FROM c.network
        AND m.construction IS NOT DISTINCT FROM c.construction
        AND m.is_bridge IS NOT DISTINCT FROM c.is_bridge
        AND m.is_tunnel IS NOT DISTINCT FROM c.is_tunnel
        AND m.is_ford IS NOT DISTINCT FROM c.is_ford
        AND m.expressway IS NOT DISTINCT FROM c.expressway
        AND m.bicycle IS NOT DISTINCT FROM c.bicycle
        AND m.foot IS NOT DISTINCT FROM c.foot
        AND m.horse IS NOT DISTINCT FROM c.horse
        AND m.mtb_scale IS NOT DISTINCT FROM c.mtb_scale
        AND m.sac_scale IS NOT DISTINCT FROM c.sac_scale
        AND m.access IS NOT DISTINCT FROM c.access
        AND m.toll IS NOT DISTINCT FROM c.toll
        AND m.layer IS NOT DISTINCT FROM c.layer
    ;

    INSERT INTO osm_transportation_merge_linestring_gen_z11(geometry, osm_id, highway, network, construction, is_bridge, is_tunnel, is_ford, expressway, z_order, bicycle, foot, horse, mtb_scale, sac_scale, access, toll, layer)
    SELECT (ST_Dump(ST_LineMerge(ST_Collect(geometry)))).geom AS geometry,
        NULL::bigint AS osm_id,
        highway,
        network,
        construction,
        is_bridge,
        is_tunnel,
        is_ford,
        expressway,
        min(z_order) as z_order,
        bicycle,
        foot,
        horse,
        mtb_scale,
        sac_scale,
        CASE
            WHEN access IN ('private', 'no') THEN 'no'
            ELSE NULL::text END AS access,
        toll,
        layer
    FROM ((
        SELECT * FROM osm_highway_linestring_original
    ) UNION ALL (
        -- New or updated ways
        SELECT
            *
        FROM
            changes_compact
        WHERE
            NOT is_old
    )) AS t
    GROUP BY highway, network, construction, is_bridge, is_tunnel, is_ford, expressway, bicycle, foot, horse, mtb_scale, sac_scale, access, toll, layer
    ;

    DROP TABLE osm_highway_linestring_original;
    DROP TABLE changes_compact;
    -- noinspection SqlWithoutWhere
    DELETE FROM transportation.changes_z11;
    -- noinspection SqlWithoutWhere
    DELETE FROM transportation.updates_z11;

    RAISE LOG 'Refresh transportation z11 done in %', age(clock_timestamp(), t);
    RETURN NULL;
END;
$$ LANGUAGE plpgsql;


CREATE TRIGGER trigger_store_transportation_highway_linestring_gen_z11
    AFTER INSERT OR UPDATE OR DELETE
    ON osm_highway_linestring_gen_z11
    FOR EACH ROW
EXECUTE PROCEDURE transportation.store_z11();

CREATE TRIGGER trigger_flag_transportation_z11
    AFTER INSERT OR UPDATE OR DELETE
    ON osm_highway_linestring_gen_z11
    FOR EACH STATEMENT
EXECUTE PROCEDURE transportation.flag_z11();

CREATE CONSTRAINT TRIGGER trigger_refresh_z11
    AFTER INSERT
    ON transportation.updates_z11
    INITIALLY DEFERRED
    FOR EACH ROW
EXECUTE PROCEDURE transportation.refresh_z11();


-- Handle updates on
-- osm_transportation_merge_linestring_gen_z11 -> osm_transportation_merge_linestring_gen_z10
-- osm_transportation_merge_linestring_gen_z11 -> osm_transportation_merge_linestring_gen_z9


CREATE OR REPLACE FUNCTION transportation.merge_linestring_gen_refresh_z10() RETURNS trigger AS
$$
BEGIN
    IF (tg_op = 'DELETE') THEN
        DELETE FROM osm_transportation_merge_linestring_gen_z10 WHERE id = old.id;
        DELETE FROM osm_transportation_merge_linestring_gen_z9 WHERE id = old.id;
    END IF;

    IF (tg_op = 'UPDATE' OR tg_op = 'INSERT') THEN
        PERFORM insert_transportation_merge_linestring_gen_z10(new.id);
    END IF;

    RETURN NULL;
END;
$$ LANGUAGE plpgsql;


CREATE TRIGGER trigger_osm_transportation_merge_linestring_gen_z11
    AFTER INSERT OR UPDATE OR DELETE
    ON osm_transportation_merge_linestring_gen_z11
    FOR EACH ROW
EXECUTE PROCEDURE transportation.merge_linestring_gen_refresh_z10();


-- Handle updates on
-- osm_transportation_merge_linestring_gen_z9 -> osm_transportation_merge_linestring_gen_z8


CREATE TABLE IF NOT EXISTS transportation.changes_z9
(
    is_old boolean,
    geometry geometry,
    id bigint,
    highway character varying,
    network character varying,
    construction character varying,
    is_bridge boolean,
    is_tunnel boolean,
    is_ford boolean,
    expressway boolean,
    z_order integer
);

CREATE OR REPLACE FUNCTION transportation.store_z9() RETURNS trigger AS
$$
BEGIN
    IF (tg_op = 'DELETE' OR tg_op = 'UPDATE') THEN
        INSERT INTO transportation.changes_z9(is_old, geometry, id, highway, network, construction, is_bridge, is_tunnel, is_ford, expressway, z_order)
        VALUES (true, old.geometry, old.id, old.highway, old.network, old.construction, old.is_bridge, old.is_tunnel, old.is_ford, old.expressway, old.z_order);
    END IF;
    IF (tg_op = 'UPDATE' OR tg_op = 'INSERT') THEN
        INSERT INTO transportation.changes_z9(is_old, geometry, id, highway, network, construction, is_bridge, is_tunnel, is_ford, expressway, z_order)
        VALUES (false, new.geometry, new.id, new.highway, new.network, new.construction, new.is_bridge, new.is_tunnel, new.is_ford, new.expressway, new.z_order);
    END IF;
    RETURN NULL;
END;
$$ LANGUAGE plpgsql;

CREATE TABLE IF NOT EXISTS transportation.updates_z9
(
    id serial PRIMARY KEY,
    t text,
    UNIQUE (t)
);
CREATE OR REPLACE FUNCTION transportation.flag_z9() RETURNS trigger AS
$$
BEGIN
    INSERT INTO transportation.updates_z9(t) VALUES ('y') ON CONFLICT(t) DO NOTHING;
    RETURN NULL;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION transportation.refresh_z8() RETURNS trigger AS
$$
DECLARE
    t TIMESTAMP WITH TIME ZONE := clock_timestamp();
BEGIN
    RAISE LOG 'Refresh transportation z9';

    -- Compact the change history to keep only the first and last version
    CREATE TEMP TABLE changes_compact AS
    SELECT
        *
    FROM ((
        SELECT DISTINCT ON (id) *
        FROM transportation.changes_z9
        WHERE is_old
        ORDER BY id,
                 id ASC
    ) UNION ALL (
        SELECT DISTINCT ON (id) *
        FROM transportation.changes_z9
        WHERE NOT is_old
        ORDER BY id,
                 id DESC
    )) AS t;

    -- Collect all original existing ways from impacted mmerge
    CREATE TEMP TABLE osm_highway_linestring_original AS
    SELECT DISTINCT ON (h.id)
        NULL::boolean AS is_old,
        h.geometry,
        h.id,
        h.highway,
        h.network,
        h.construction,
        h.is_bridge,
        h.is_tunnel,
        h.is_ford,
        h.expressway,
        h.z_order
    FROM
        changes_compact AS c
        JOIN osm_transportation_merge_linestring_gen_z8 AS m ON
             m.geometry && c.geometry
             AND m.highway IS NOT DISTINCT FROM c.highway
             AND m.network IS NOT DISTINCT FROM c.network
             AND m.construction IS NOT DISTINCT FROM c.construction
             AND m.is_bridge IS NOT DISTINCT FROM c.is_bridge
             AND m.is_tunnel IS NOT DISTINCT FROM c.is_tunnel
             AND m.is_ford IS NOT DISTINCT FROM c.is_ford
             AND m.expressway IS NOT DISTINCT FROM c.expressway
        JOIN osm_transportation_merge_linestring_gen_z9 AS h ON
             h.geometry && c.geometry
             AND h.id NOT IN (SELECT id FROM changes_compact)
             AND ST_Contains(m.geometry, h.geometry)
             AND h.highway IS NOT DISTINCT FROM m.highway
             AND h.network IS NOT DISTINCT FROM m.network
             AND h.construction IS NOT DISTINCT FROM m.construction
             AND h.is_bridge IS NOT DISTINCT FROM m.is_bridge
             AND h.is_tunnel IS NOT DISTINCT FROM m.is_tunnel
             AND h.is_ford IS NOT DISTINCT FROM m.is_ford
             AND h.expressway IS NOT DISTINCT FROM m.expressway
    ORDER BY
        h.id
    ;

    DELETE
    FROM osm_transportation_merge_linestring_gen_z8 AS m
        USING changes_compact AS c
    WHERE
        m.geometry && c.geometry
        AND m.highway IS NOT DISTINCT FROM c.highway
        AND m.network IS NOT DISTINCT FROM c.network
        AND m.construction IS NOT DISTINCT FROM c.construction
        AND m.is_bridge IS NOT DISTINCT FROM c.is_bridge
        AND m.is_tunnel IS NOT DISTINCT FROM c.is_tunnel
        AND m.is_ford IS NOT DISTINCT FROM c.is_ford
        AND m.expressway IS NOT DISTINCT FROM c.expressway
    ;

    INSERT INTO osm_transportation_merge_linestring_gen_z8(geometry, osm_id, highway, network, construction, is_bridge, is_tunnel, is_ford, expressway, z_order)
    SELECT (ST_Dump(ST_LineMerge(ST_Collect(geometry)))).geom AS geometry,
        NULL::bigint AS osm_id,
        highway,
        network,
        construction,
        is_bridge,
        is_tunnel,
        is_ford,
        expressway,
        min(z_order) as z_order
    FROM ((
        SELECT * FROM osm_highway_linestring_original
    ) UNION ALL (
        -- New or updated ways
        SELECT
            *
        FROM
            changes_compact
        WHERE
            NOT is_old
    )) AS t
    GROUP BY highway, network, construction, is_bridge, is_tunnel, is_ford, expressway
    ;

    DROP TABLE osm_highway_linestring_original;
    DROP TABLE changes_compact;
    -- noinspection SqlWithoutWhere
    DELETE FROM transportation.changes_z9;
    -- noinspection SqlWithoutWhere
    DELETE FROM transportation.updates_z9;

    RAISE LOG 'Refresh transportation z9 done in %', age(clock_timestamp(), t);
    RETURN NULL;
END;
$$ LANGUAGE plpgsql;


CREATE TRIGGER trigger_store_transportation_highway_linestring_gen_z9
    AFTER INSERT OR UPDATE OR DELETE
    ON osm_transportation_merge_linestring_gen_z9
    FOR EACH ROW
EXECUTE PROCEDURE transportation.store_z9();

CREATE TRIGGER trigger_flag_transportation_z9
    AFTER INSERT OR UPDATE OR DELETE
    ON osm_transportation_merge_linestring_gen_z9
    FOR EACH STATEMENT
EXECUTE PROCEDURE transportation.flag_z9();

CREATE CONSTRAINT TRIGGER trigger_refresh_z8
    AFTER INSERT
    ON transportation.updates_z9
    INITIALLY DEFERRED
    FOR EACH ROW
EXECUTE PROCEDURE transportation.refresh_z8();


-- Handle updates on
-- osm_transportation_merge_linestring_gen_z8 -> osm_transportation_merge_linestring_gen_z7
-- osm_transportation_merge_linestring_gen_z8 -> osm_transportation_merge_linestring_gen_z6
-- osm_transportation_merge_linestring_gen_z8 -> osm_transportation_merge_linestring_gen_z5
-- osm_transportation_merge_linestring_gen_z8 -> osm_transportation_merge_linestring_gen_z4


CREATE OR REPLACE FUNCTION transportation.merge_linestring_gen_refresh_z7() RETURNS trigger AS
$$
BEGIN
    IF (tg_op = 'DELETE') THEN
        DELETE FROM osm_transportation_merge_linestring_gen_z7 WHERE id = old.id;
        DELETE FROM osm_transportation_merge_linestring_gen_z6 WHERE id = old.id;
        DELETE FROM osm_transportation_merge_linestring_gen_z5 WHERE id = old.id;
        DELETE FROM osm_transportation_merge_linestring_gen_z4 WHERE id = old.id;
    END IF;

    IF (tg_op = 'UPDATE' OR tg_op = 'INSERT') THEN
        PERFORM insert_transportation_merge_linestring_gen_z7(new.id);
    END IF;

    RETURN NULL;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER trigger_osm_transportation_merge_linestring_gen_z8
    AFTER INSERT OR UPDATE OR DELETE
    ON osm_transportation_merge_linestring_gen_z8
    FOR EACH ROW
EXECUTE PROCEDURE transportation.merge_linestring_gen_refresh_z7();

-- Layer transportation - ./transportation.sql

CREATE OR REPLACE FUNCTION highway_is_link(highway text) RETURNS boolean AS
$$
SELECT highway LIKE '%_link';
$$ LANGUAGE SQL IMMUTABLE
                STRICT
                PARALLEL SAFE;


-- etldoc: layer_transportation[shape=record fillcolor=lightpink, style="rounded,filled",
-- etldoc:     label="<sql> layer_transportation |<z4> z4 |<z5> z5 |<z6> z6 |<z7> z7 |<z8> z8 |<z9> z9 |<z10> z10 |<z11> z11 |<z12> z12|<z13> z13|<z14_> z14+" ] ;
CREATE OR REPLACE FUNCTION layer_transportation(bbox geometry, zoom_level int)
    RETURNS TABLE
            (
                osm_id     bigint,
                geometry   geometry,
                class      text,
                subclass   text,
                network    text,
                ramp       int,
                oneway     int,
                brunnel    text,
                service    text,
                access     text,
                toll       int,
                expressway int,
                layer      int,
                level      int,
                indoor     int,
                bicycle    text,
                foot       text,
                horse      text,
                mtb_scale  text,
                surface    text
            )
AS
$$
SELECT osm_id,
       geometry,
       CASE
           WHEN highway <> '' OR public_transport <> ''
               THEN highway_class(highway, public_transport, construction)
           WHEN railway <> '' THEN railway_class(railway)
           WHEN aerialway <> '' THEN 'aerialway'
           WHEN shipway <> '' THEN shipway
           WHEN man_made <> '' THEN man_made
           END AS class,
       CASE
           WHEN railway IS NOT NULL THEN railway
           WHEN (highway IS NOT NULL OR public_transport IS NOT NULL)
               AND highway_class(highway, public_transport, construction) = 'path'
               THEN COALESCE(NULLIF(public_transport, ''), highway)
           WHEN aerialway IS NOT NULL THEN aerialway
           END AS subclass,
       NULLIF(network, '') AS network,
       -- All links are considered as ramps as well
       CASE
           WHEN highway_is_link(highway)
             OR is_ramp
               THEN 1 END AS ramp,
       CASE WHEN is_oneway <> 0 THEN is_oneway::int END AS oneway,
       brunnel(is_bridge, is_tunnel, is_ford) AS brunnel,
       NULLIF(service, '') AS service,
       access,
       CASE WHEN toll = TRUE THEN 1 END AS toll,
       CASE WHEN highway NOT IN ('', 'motorway') AND expressway = TRUE THEN 1 END AS expressway,
       NULLIF(layer, 0) AS layer,
       "level",
       CASE WHEN indoor = TRUE THEN 1 END AS indoor,
       NULLIF(bicycle, '') AS bicycle,
       NULLIF(foot, '') AS foot,
       NULLIF(horse, '') AS horse,
       NULLIF(mtb_scale, '') AS mtb_scale,
       NULLIF(surface, '') AS surface
FROM (
         -- etldoc: osm_transportation_merge_linestring_gen_z4 -> layer_transportation:z4
         SELECT osm_id,
                geometry,
                highway,
                construction,
                network,
                NULL AS railway,
                NULL AS aerialway,
                NULL AS shipway,
                NULL AS public_transport,
                NULL AS service,
                NULL AS access,
                NULL::boolean AS toll,
                is_bridge,
                is_tunnel,
                is_ford,
                NULL::boolean AS expressway,
                NULL::boolean AS is_ramp,
                NULL::int AS is_oneway,
                NULL AS man_made,
                NULL::int AS layer,
                NULL::int AS level,
                NULL::boolean AS indoor,
                NULL AS bicycle,
                NULL AS foot,
                NULL AS horse,
                NULL AS mtb_scale,
                NULL AS surface,
                z_order
         FROM osm_transportation_merge_linestring_gen_z4
         WHERE zoom_level = 4
         UNION ALL

         -- etldoc: osm_transportation_merge_linestring_gen_z5 -> layer_transportation:z5
         SELECT osm_id,
                geometry,
                highway,
                construction,
                network,
                NULL AS railway,
                NULL AS aerialway,
                NULL AS shipway,
                NULL AS public_transport,
                NULL AS service,
                NULL AS access,
                NULL::boolean AS toll,
                is_bridge,
                is_tunnel,
                is_ford,
                NULL::boolean AS expressway,
                NULL::boolean AS is_ramp,
                NULL::int AS is_oneway,
                NULL AS man_made,
                NULL::int AS layer,
                NULL::int AS level,
                NULL::boolean AS indoor,
                NULL AS bicycle,
                NULL AS foot,
                NULL AS horse,
                NULL AS mtb_scale,
                NULL AS surface,
                z_order
         FROM osm_transportation_merge_linestring_gen_z5
         WHERE zoom_level = 5
         UNION ALL

         -- etldoc: osm_transportation_merge_linestring_gen_z6 -> layer_transportation:z6
         SELECT osm_id,
                geometry,
                highway,
                construction,
                network,
                NULL AS railway,
                NULL AS aerialway,
                NULL AS shipway,
                NULL AS public_transport,
                NULL AS service,
                NULL AS access,
                NULL::boolean AS toll,
                is_bridge,
                is_tunnel,
                is_ford,
                NULL::boolean AS expressway,
                NULL::boolean AS is_ramp,
                NULL::int AS is_oneway,
                NULL AS man_made,
                NULL::int AS layer,
                NULL::int AS level,
                NULL::boolean AS indoor,
                NULL AS bicycle,
                NULL AS foot,
                NULL AS horse,
                NULL AS mtb_scale,
                NULL AS surface,
                z_order
         FROM osm_transportation_merge_linestring_gen_z6
         WHERE zoom_level = 6
         UNION ALL

         -- etldoc: osm_transportation_merge_linestring_gen_z7  ->  layer_transportation:z7
         SELECT osm_id,
                geometry,
                highway,
                construction,
                network,
                NULL AS railway,
                NULL AS aerialway,
                NULL AS shipway,
                NULL AS public_transport,
                NULL AS service,
                NULL AS access,
                NULL::boolean AS toll,
                is_bridge,
                is_tunnel,
                is_ford,
                expressway,
                NULL::boolean AS is_ramp,
                NULL::int AS is_oneway,
                NULL AS man_made,
                NULL::int AS layer,
                NULL::int AS level,
                NULL::boolean AS indoor,
                NULL AS bicycle,
                NULL AS foot,
                NULL AS horse,
                NULL AS mtb_scale,
                NULL AS surface,
                z_order
         FROM osm_transportation_merge_linestring_gen_z7
         WHERE zoom_level = 7
         UNION ALL

         -- etldoc: osm_transportation_merge_linestring_gen_z8  ->  layer_transportation:z8
         SELECT osm_id,
                geometry,
                highway,
                construction,
                network,
                NULL AS railway,
                NULL AS aerialway,
                NULL AS shipway,
                NULL AS public_transport,
                NULL AS service,
                NULL AS access,
                NULL::boolean AS toll,
                is_bridge,
                is_tunnel,
                is_ford,
                expressway,
                NULL::boolean AS is_ramp,
                NULL::int AS is_oneway,
                NULL AS man_made,
                NULL::int AS layer,
                NULL::int AS level,
                NULL::boolean AS indoor,
                NULL AS bicycle,
                NULL AS foot,
                NULL AS horse,
                NULL AS mtb_scale,
                NULL AS surface,
                z_order
         FROM osm_transportation_merge_linestring_gen_z8
         WHERE zoom_level = 8
         UNION ALL

         -- etldoc: osm_transportation_merge_linestring_gen_z9  ->  layer_transportation:z9
         SELECT osm_id,
                geometry,
                highway,
                construction,
                network,
                NULL AS railway,
                NULL AS aerialway,
                NULL AS shipway,
                NULL AS public_transport,
                NULL AS service,
                access,
                toll,
                is_bridge,
                is_tunnel,
                is_ford,
                expressway,
                NULL::boolean AS is_ramp,
                NULL::int AS is_oneway,
                NULL AS man_made,
                layer,
                NULL::int AS level,
                NULL::boolean AS indoor,
                bicycle,
                foot,
                horse,
                mtb_scale,
                NULL AS surface,
                z_order
         FROM osm_transportation_merge_linestring_gen_z9
         WHERE zoom_level = 9
         UNION ALL

         -- etldoc: osm_transportation_merge_linestring_gen_z10  ->  layer_transportation:z10
         SELECT osm_id,
                geometry,
                highway,
                construction,
                network,
                NULL AS railway,
                NULL AS aerialway,
                NULL AS shipway,
                NULL AS public_transport,
                NULL AS service,
                access,
                toll,
                is_bridge,
                is_tunnel,
                is_ford,
                expressway,
                NULL::boolean AS is_ramp,
                NULL::int AS is_oneway,
                NULL AS man_made,
                layer,
                NULL::int AS level,
                NULL::boolean AS indoor,
                bicycle,
                foot,
                horse,
                mtb_scale,
                NULL AS surface,
                z_order
         FROM osm_transportation_merge_linestring_gen_z10
         WHERE zoom_level = 10
         UNION ALL

         -- etldoc: osm_transportation_merge_linestring_gen_z11  ->  layer_transportation:z11
         SELECT osm_id,
                geometry,
                highway,
                construction,
                network,
                NULL AS railway,
                NULL AS aerialway,
                NULL AS shipway,
                NULL AS public_transport,
                NULL AS service,
                access,
                toll,
                is_bridge,
                is_tunnel,
                is_ford,
                expressway,
                NULL::boolean AS is_ramp,
                NULL::int AS is_oneway,
                NULL AS man_made,
                layer,
                NULL::int AS level,
                NULL::boolean AS indoor,
                bicycle,
                foot,
                horse,
                mtb_scale,
                NULL AS surface,
                z_order
         FROM osm_transportation_merge_linestring_gen_z11
         WHERE zoom_level = 11
         UNION ALL

         -- etldoc: osm_highway_linestring  ->  layer_transportation:z12
         -- etldoc: osm_highway_linestring  ->  layer_transportation:z13
         -- etldoc: osm_highway_linestring  ->  layer_transportation:z14_
         -- etldoc: osm_transportation_name_network  ->  layer_transportation:z12
         -- etldoc: osm_transportation_name_network  ->  layer_transportation:z13
         -- etldoc: osm_transportation_name_network  ->  layer_transportation:z14_
         SELECT hl.osm_id,
                hl.geometry,
                hl.highway,
                construction,
                network,
                NULL AS railway,
                NULL AS aerialway,
                NULL AS shipway,
                public_transport,
                service_value(service) AS service,
                CASE WHEN access IN ('private', 'no') THEN 'no' END AS access,
                toll,
                is_bridge,
                is_tunnel,
                is_ford,
                expressway,
                is_ramp,
                is_oneway,
                man_made,
                hl.layer,
                CASE WHEN hl.highway IN ('footway', 'steps') THEN hl.level END AS level,
                CASE WHEN hl.highway IN ('footway', 'steps') THEN hl.indoor END AS indoor,
                bicycle,
                foot,
                horse,
                mtb_scale,
                surface_value(COALESCE(NULLIF(surface, ''), tracktype)) AS "surface",
                hl.z_order
         FROM osm_highway_linestring hl
         LEFT OUTER JOIN osm_transportation_name_network n ON hl.osm_id = n.osm_id
         WHERE NOT is_area
           AND
               CASE WHEN zoom_level = 12 THEN
                         CASE WHEN transportation_filter_z12(hl.highway, hl.construction) THEN TRUE
                              WHEN hl.highway IN ('track', 'path') THEN n.route_rank = 1
                         END
                    WHEN zoom_level = 13 THEN
                         CASE WHEN man_made='pier' THEN NOT ST_IsClosed(hl.geometry)
                              WHEN hl.highway IN ('track', 'path') THEN (hl.name <> ''
                                                                   OR n.route_rank BETWEEN 1 AND 2
                                                                   OR hl.sac_scale <> ''
                                                                   )
                              ELSE transportation_filter_z13(hl.highway, public_transport, hl.construction, service)
                         END
                    WHEN zoom_level >= 14 THEN
                         CASE WHEN man_made='pier' THEN NOT ST_IsClosed(hl.geometry)
                              ELSE TRUE
                         END
               END
         UNION ALL

         -- etldoc: osm_railway_linestring_gen_z8  ->  layer_transportation:z8
         SELECT osm_id,
                geometry,
                NULL AS highway,
                NULL AS construction,
                NULL AS network,
                railway,
                NULL AS aerialway,
                NULL AS shipway,
                NULL AS public_transport,
                service_value(service) AS service,
                NULL::text AS access,
                NULL::boolean AS toll,
                NULL::boolean AS is_bridge,
                NULL::boolean AS is_tunnel,
                NULL::boolean AS is_ford,
                NULL::boolean AS expressway,
                NULL::boolean AS is_ramp,
                NULL::int AS is_oneway,
                NULL AS man_made,
                NULL::int AS layer,
                NULL::int AS level,
                NULL::boolean AS indoor,
                NULL AS bicycle,
                NULL AS foot,
                NULL AS horse,
                NULL AS mtb_scale,
                NULL AS surface,
                z_order
         FROM osm_railway_linestring_gen_z8
         WHERE zoom_level = 8
           AND railway = 'rail'
           AND service = ''
           AND usage = 'main'
         UNION ALL

         -- etldoc: osm_railway_linestring_gen_z9  ->  layer_transportation:z9
         SELECT osm_id,
                geometry,
                NULL AS highway,
                NULL AS construction,
                NULL AS network,
                railway,
                NULL AS aerialway,
                NULL AS shipway,
                NULL AS public_transport,
                service_value(service) AS service,
                NULL::text AS access,
                NULL::boolean AS toll,
                NULL::boolean AS is_bridge,
                NULL::boolean AS is_tunnel,
                NULL::boolean AS is_ford,
                NULL::boolean AS expressway,
                NULL::boolean AS is_ramp,
                NULL::int AS is_oneway,
                NULL AS man_made,
                layer,
                NULL::int AS level,
                NULL::boolean AS indoor,
                NULL AS bicycle,
                NULL AS foot,
                NULL AS horse,
                NULL AS mtb_scale,
                NULL AS surface,
                z_order
         FROM osm_railway_linestring_gen_z9
         WHERE zoom_level = 9
           AND railway = 'rail'
           AND service = ''
           AND usage = 'main'
         UNION ALL

         -- etldoc: osm_railway_linestring_gen_z10  ->  layer_transportation:z10
         SELECT osm_id,
                geometry,
                NULL AS highway,
                NULL AS construction,
                NULL AS network,
                railway,
                NULL AS aerialway,
                NULL AS shipway,
                NULL AS public_transport,
                service_value(service) AS service,
                NULL::text AS access,
                NULL::boolean AS toll,
                is_bridge,
                is_tunnel,
                is_ford,
                NULL::boolean AS expressway,
                is_ramp,
                is_oneway,
                NULL AS man_made,
                layer,
                NULL::int AS level,
                NULL::boolean AS indoor,
                NULL AS bicycle,
                NULL AS foot,
                NULL AS horse,
                NULL AS mtb_scale,
                NULL AS surface,
                z_order
         FROM osm_railway_linestring_gen_z10
         WHERE zoom_level = 10
           AND railway IN ('rail', 'narrow_gauge')
           AND service = ''
         UNION ALL

         -- etldoc: osm_railway_linestring_gen_z11  ->  layer_transportation:z11
         SELECT osm_id,
                geometry,
                NULL AS highway,
                NULL AS construction,
                NULL AS network,
                railway,
                NULL AS aerialway,
                NULL AS shipway,
                NULL AS public_transport,
                service_value(service) AS service,
                NULL::text AS access,
                NULL::boolean AS toll,
                is_bridge,
                is_tunnel,
                is_ford,
                NULL::boolean AS expressway,
                is_ramp,
                is_oneway,
                NULL AS man_made,
                layer,
                NULL::int AS level,
                NULL::boolean AS indoor,
                NULL AS bicycle,
                NULL AS foot,
                NULL AS horse,
                NULL AS mtb_scale,
                NULL AS surface,
                z_order
         FROM osm_railway_linestring_gen_z11
         WHERE zoom_level = 11
           AND railway IN ('rail', 'narrow_gauge', 'light_rail')
           AND service = ''
         UNION ALL

         -- etldoc: osm_railway_linestring_gen_z12  ->  layer_transportation:z12
         SELECT osm_id,
                geometry,
                NULL AS highway,
                NULL AS construction,
                NULL AS network,
                railway,
                NULL AS aerialway,
                NULL AS shipway,
                NULL AS public_transport,
                service_value(service) AS service,
                NULL::text AS access,
                NULL::boolean AS toll,
                is_bridge,
                is_tunnel,
                is_ford,
                NULL::boolean AS expressway,
                is_ramp,
                is_oneway,
                NULL AS man_made,
                layer,
                NULL::int AS level,
                NULL::boolean AS indoor,
                NULL AS bicycle,
                NULL AS foot,
                NULL AS horse,
                NULL AS mtb_scale,
                NULL AS surface,
                z_order
         FROM osm_railway_linestring_gen_z12
         WHERE zoom_level = 12
           AND railway IN ('rail', 'narrow_gauge', 'light_rail')
           AND service = ''
         UNION ALL

         -- etldoc: osm_railway_linestring ->  layer_transportation:z13
         -- etldoc: osm_railway_linestring ->  layer_transportation:z14_
         SELECT osm_id,
                geometry,
                NULL AS highway,
                NULL AS construction,
                NULL AS network,
                railway,
                NULL AS aerialway,
                NULL AS shipway,
                NULL AS public_transport,
                service_value(service) AS service,
                NULL::text AS access,
                NULL::boolean AS toll,
                is_bridge,
                is_tunnel,
                is_ford,
                NULL::boolean AS expressway,
                is_ramp,
                is_oneway,
                NULL AS man_made,
                layer,
                NULL::int AS level,
                NULL::boolean AS indoor,
                NULL AS bicycle,
                NULL AS foot,
                NULL AS horse,
                NULL AS mtb_scale,
                NULL AS surface,
                z_order
         FROM osm_railway_linestring
         WHERE zoom_level = 13
           AND railway IN ('rail', 'narrow_gauge', 'light_rail')
           AND service = ''
           OR zoom_level >= 14
         UNION ALL

         -- etldoc: osm_aerialway_linestring_gen_z12  ->  layer_transportation:z12
         SELECT osm_id,
                geometry,
                NULL AS highway,
                NULL AS construction,
                NULL AS network,
                NULL AS railway,
                aerialway,
                NULL AS shipway,
                NULL AS public_transport,
                service_value(service) AS service,
                NULL::text AS access,
                NULL::boolean AS toll,
                is_bridge,
                is_tunnel,
                is_ford,
                NULL::boolean AS expressway,
                is_ramp,
                is_oneway,
                NULL AS man_made,
                layer,
                NULL::int AS level,
                NULL::boolean AS indoor,
                NULL AS bicycle,
                NULL AS foot,
                NULL AS horse,
                NULL AS mtb_scale,
                NULL AS surface,
                z_order
         FROM osm_aerialway_linestring_gen_z12
         WHERE zoom_level = 12
         UNION ALL

         -- etldoc: osm_aerialway_linestring ->  layer_transportation:z13
         -- etldoc: osm_aerialway_linestring ->  layer_transportation:z14_
         SELECT osm_id,
                geometry,
                NULL AS highway,
                NULL AS construction,
                NULL AS network,
                NULL AS railway,
                aerialway,
                NULL AS shipway,
                NULL AS public_transport,
                service_value(service) AS service,
                NULL::text AS access,
                NULL::boolean AS toll,
                is_bridge,
                is_tunnel,
                is_ford,
                NULL::boolean AS expressway,
                is_ramp,
                is_oneway,
                NULL AS man_made,
                layer,
                NULL::int AS level,
                NULL::boolean AS indoor,
                NULL AS bicycle,
                NULL AS foot,
                NULL AS horse,
                NULL AS mtb_scale,
                NULL AS surface,
                z_order
         FROM osm_aerialway_linestring
         WHERE zoom_level >= 13
         UNION ALL

         -- etldoc: osm_shipway_linestring_gen_z11  ->  layer_transportation:z11
         SELECT osm_id,
                geometry,
                NULL AS highway,
                NULL AS construction,
                NULL AS network,
                NULL AS railway,
                NULL AS aerialway,
                shipway,
                NULL AS public_transport,
                service_value(service) AS service,
                NULL::text AS access,
                NULL::boolean AS toll,
                is_bridge,
                is_tunnel,
                is_ford,
                NULL::boolean AS expressway,
                is_ramp,
                is_oneway,
                NULL AS man_made,
                layer,
                NULL::int AS level,
                NULL::boolean AS indoor,
                NULL AS bicycle,
                NULL AS foot,
                NULL AS horse,
                NULL AS mtb_scale,
                NULL AS surface,
                z_order
         FROM osm_shipway_linestring_gen_z11
         WHERE zoom_level = 11
         UNION ALL

         -- etldoc: osm_shipway_linestring_gen_z12  ->  layer_transportation:z12
         SELECT osm_id,
                geometry,
                NULL AS highway,
                NULL AS construction,
                NULL AS network,
                NULL AS railway,
                NULL AS aerialway,
                shipway,
                NULL AS public_transport,
                service_value(service) AS service,
                NULL::text AS access,
                NULL::boolean AS toll,
                is_bridge,
                is_tunnel,
                is_ford,
                NULL::boolean AS expressway,
                is_ramp,
                is_oneway,
                NULL AS man_made,
                layer,
                NULL::int AS level,
                NULL::boolean AS indoor,
                NULL AS bicycle,
                NULL AS foot,
                NULL AS horse,
                NULL AS mtb_scale,
                NULL AS surface,
                z_order
         FROM osm_shipway_linestring_gen_z12
         WHERE zoom_level = 12
         UNION ALL

         -- etldoc: osm_shipway_linestring ->  layer_transportation:z13
         -- etldoc: osm_shipway_linestring ->  layer_transportation:z14_
         SELECT osm_id,
                geometry,
                NULL AS highway,
                NULL AS construction,
                NULL AS network,
                NULL AS railway,
                NULL AS aerialway,
                shipway,
                NULL AS public_transport,
                service_value(service) AS service,
                NULL::text AS access,
                NULL::boolean AS toll,
                is_bridge,
                is_tunnel,
                is_ford,
                NULL::boolean AS expressway,
                is_ramp,
                is_oneway,
                NULL AS man_made,
                layer,
                NULL::int AS level,
                NULL::boolean AS indoor,
                NULL AS bicycle,
                NULL AS foot,
                NULL AS horse,
                NULL AS mtb_scale,
                NULL AS surface,
                z_order
         FROM osm_shipway_linestring
         WHERE zoom_level >= 13
         UNION ALL

         -- NOTE: We limit the selection of polys because we need to be
         -- careful to net get false positives here because
         -- it is possible that closed linestrings appear both as
         -- highway linestrings and as polygon
         -- etldoc: osm_highway_polygon ->  layer_transportation:z13
         -- etldoc: osm_highway_polygon ->  layer_transportation:z14_
         SELECT osm_id,
                geometry,
                highway,
                NULL AS construction,
                NULL AS network,
                NULL AS railway,
                NULL AS aerialway,
                NULL AS shipway,
                public_transport,
                NULL AS service,
                NULL::text AS access,
                NULL::boolean AS toll,
                CASE
                    WHEN man_made IN ('bridge') THEN TRUE
                    ELSE FALSE
                    END AS is_bridge,
                FALSE AS is_tunnel,
                FALSE AS is_ford,
                NULL::boolean AS expressway,
                FALSE AS is_ramp,
                FALSE::int AS is_oneway,
                man_made,
                layer,
                NULL::int AS level,
                NULL::boolean AS indoor,
                NULL AS bicycle,
                NULL AS foot,
                NULL AS horse,
                NULL AS mtb_scale,
                NULL AS surface,
                z_order
         FROM osm_highway_polygon
              -- We do not want underground pedestrian areas for now
         WHERE zoom_level >= 13
           AND (
                 man_made IN ('bridge', 'pier')
                 OR (is_area AND COALESCE(layer, 0) >= 0)
             )
     ) AS zoom_levels
WHERE geometry && bbox
ORDER BY z_order ASC;
$$ LANGUAGE SQL STABLE
                -- STRICT
                PARALLEL SAFE;

DO $$ BEGIN RAISE NOTICE 'Finished layer transportation'; END$$;

DO $$ BEGIN RAISE NOTICE 'Processing layer transportation_name'; END$$;

-- Layer transportation_name - ./highway_classification.sql

CREATE OR REPLACE FUNCTION highway_to_val(hwy_class varchar)
RETURNS int
IMMUTABLE
LANGUAGE plpgsql
AS $$
BEGIN
  CASE hwy_class
    WHEN 'motorway'     THEN RETURN 6;
    WHEN 'trunk'        THEN RETURN 5;
    WHEN 'primary'      THEN RETURN 4;
    WHEN 'secondary'    THEN RETURN 3;
    WHEN 'tertiary'     THEN RETURN 2;
    WHEN 'unclassified' THEN RETURN 1;
    else RETURN 0;
  END CASE;
END;
$$;

CREATE OR REPLACE FUNCTION val_to_highway(hwy_val int)
RETURNS varchar
IMMUTABLE
LANGUAGE plpgsql
AS $$
BEGIN
  CASE hwy_val
    WHEN 6 THEN RETURN 'motorway';
    WHEN 5 THEN RETURN 'trunk';
    WHEN 4 THEN RETURN 'primary';
    WHEN 3 THEN RETURN 'secondary';
    WHEN 2 THEN RETURN 'tertiary';
    WHEN 1 THEN RETURN 'unclassified';
    else RETURN null;
  END CASE;
END;
$$;

CREATE OR REPLACE FUNCTION highest_hwy_sfunc(agg_state varchar, hwy_class varchar)
RETURNS varchar
IMMUTABLE
LANGUAGE plpgsql
AS $$
BEGIN
  RETURN val_to_highway(
    GREATEST(
      highway_to_val(agg_state),
      highway_to_val(hwy_class)
    )
  );
END;
$$;

DROP AGGREGATE IF EXISTS highest_highway (varchar);
CREATE AGGREGATE highest_highway (varchar)
(
    sfunc = highest_hwy_sfunc,
    stype = varchar
);

-- Layer transportation_name - ./update_transportation_name.sql

-- Instead of using relations to find out the road names we
-- stitch together the touching ways with the same name
-- to allow for nice label rendering
-- Because this works well for roads that do not have relations as well

-- etldoc: osm_transportation_name_network ->  osm_transportation_name_linestring
-- etldoc: osm_shipway_linestring ->  osm_transportation_name_linestring
-- etldoc: osm_aerialway_linestring ->  osm_transportation_name_linestring
CREATE TABLE IF NOT EXISTS osm_transportation_name_linestring AS
SELECT (ST_Dump(geometry)).geom AS geometry,
       tags || get_basic_names(tags, geometry) AS tags,
       ref,
       highway,
       subclass,
       brunnel,
       sac_scale,
       "level",
       layer,
       indoor,
       network_type AS network,
       route_1, route_2, route_3, route_4, route_5, route_6,
       z_order,
       route_rank
FROM (
         SELECT ST_LineMerge(ST_Collect(geometry)) AS geometry,
                tags,
                ref,
                highway,
                subclass,
                CASE WHEN COUNT(*) = COUNT(brunnel) AND MAX(brunnel) = MIN(brunnel)
                     THEN MAX(brunnel)
                     ELSE NULL::text END AS brunnel,
                sac_scale,
                "level",
                layer,
                indoor,
                network_type,
                route_1, route_2, route_3, route_4, route_5, route_6,
                min(z_order) AS z_order,
                min(route_rank) AS route_rank
         FROM osm_transportation_name_network
         WHERE tags->'name' <> '' OR ref <> ''
         GROUP BY tags, ref, highway, subclass, "level", layer, sac_scale, indoor, network_type,
                  route_1, route_2, route_3, route_4, route_5, route_6
         UNION ALL

         SELECT ST_LineMerge(ST_Collect(geometry)) AS geometry,
                transportation_name_tags(NULL::geometry, tags, name, name_en, name_de) AS tags,
                NULL AS ref,
                'shipway' AS highway,
                shipway AS subclass,
                NULL AS brunnel,
                NULL AS sac_scale,
                NULL::int AS level,
                layer,
                NULL AS indoor,
                NULL AS network_type,
                NULL AS route_1,
                NULL AS route_2,
                NULL AS route_3,
                NULL AS route_4,
                NULL AS route_5,
                NULL AS route_6,
                min(z_order) AS z_order,
                NULL::int AS route_rank
         FROM osm_shipway_linestring
         WHERE name <> ''
         GROUP BY name, name_en, name_de, tags, subclass, "level", layer
         UNION ALL

         SELECT ST_LineMerge(ST_Collect(geometry)) AS geometry,
                transportation_name_tags(NULL::geometry, tags, name, name_en, name_de) AS tags,
                NULL AS ref,
                'aerialway' AS highway,
                aerialway AS subclass,
                NULL AS brunnel,
                NULL AS sac_scale,
                NULL::int AS level,
                layer,
                NULL AS indoor,
                NULL AS network_type,
                NULL AS route_1,
                NULL AS route_2,
                NULL AS route_3,
                NULL AS route_4,
                NULL AS route_5,
                NULL AS route_6,
                min(z_order) AS z_order,
                NULL::int AS route_rank
         FROM osm_aerialway_linestring
         WHERE name <> ''
         GROUP BY name, name_en, name_de, tags, subclass, "level", layer
     ) AS highway_union
;
CREATE INDEX IF NOT EXISTS osm_transportation_name_linestring_name_ref_idx ON osm_transportation_name_linestring (coalesce(tags->'name', ''), coalesce(ref, ''));
CREATE INDEX IF NOT EXISTS osm_transportation_name_linestring_geometry_idx ON osm_transportation_name_linestring USING gist (geometry);

CREATE INDEX IF NOT EXISTS osm_transportation_name_linestring_highway_partial_idx
    ON osm_transportation_name_linestring (highway, subclass)
    WHERE highway IN ('motorway', 'trunk', 'construction');

-- etldoc: osm_transportation_name_linestring -> osm_transportation_name_linestring_gen1
CREATE OR REPLACE VIEW osm_transportation_name_linestring_gen1_view AS
SELECT ST_Simplify(geometry, 50) AS geometry,
       tags,
       ref,
       highway,
       subclass,
       brunnel,
       network,
       route_1, route_2, route_3, route_4, route_5, route_6,
       z_order
FROM osm_transportation_name_linestring
WHERE (highway IN ('motorway', 'trunk') OR highway = 'construction' AND subclass IN ('motorway', 'trunk'))
  AND ST_Length(geometry) > 8000
;
CREATE TABLE IF NOT EXISTS osm_transportation_name_linestring_gen1 AS
SELECT *
FROM osm_transportation_name_linestring_gen1_view;
CREATE INDEX IF NOT EXISTS osm_transportation_name_linestring_gen1_name_ref_idx ON osm_transportation_name_linestring_gen1((coalesce(tags->'name', ref)));
CREATE INDEX IF NOT EXISTS osm_transportation_name_linestring_gen1_geometry_idx ON osm_transportation_name_linestring_gen1 USING gist (geometry);

CREATE INDEX IF NOT EXISTS osm_transportation_name_linestring_gen1_highway_partial_idx
    ON osm_transportation_name_linestring_gen1 (highway, subclass)
    WHERE highway IN ('motorway', 'trunk', 'construction');

-- etldoc: osm_transportation_name_linestring_gen1 -> osm_transportation_name_linestring_gen2
CREATE OR REPLACE VIEW osm_transportation_name_linestring_gen2_view AS
SELECT ST_Simplify(geometry, 120) AS geometry,
       tags,
       ref,
       highway,
       subclass,
       brunnel,
       network,
       route_1, route_2, route_3, route_4, route_5, route_6,
       z_order
FROM osm_transportation_name_linestring_gen1
WHERE (highway IN ('motorway', 'trunk') OR highway = 'construction' AND subclass IN ('motorway', 'trunk'))
  AND ST_Length(geometry) > 14000
;
CREATE TABLE IF NOT EXISTS osm_transportation_name_linestring_gen2 AS
SELECT *
FROM osm_transportation_name_linestring_gen2_view;
CREATE INDEX IF NOT EXISTS osm_transportation_name_linestring_gen2_name_ref_idx ON osm_transportation_name_linestring_gen2((coalesce(tags->'name', ref)));
CREATE INDEX IF NOT EXISTS osm_transportation_name_linestring_gen2_geometry_idx ON osm_transportation_name_linestring_gen2 USING gist (geometry);

CREATE INDEX IF NOT EXISTS osm_transportation_name_linestring_gen2_highway_partial_idx
    ON osm_transportation_name_linestring_gen2 (highway, subclass)
    WHERE highway IN ('motorway', 'trunk', 'construction');

-- etldoc: osm_transportation_name_linestring_gen2 -> osm_transportation_name_linestring_gen3
CREATE OR REPLACE VIEW osm_transportation_name_linestring_gen3_view AS
SELECT ST_Simplify(geometry, 200) AS geometry,
       tags,
       ref,
       highway,
       subclass,
       brunnel,
       network,
       route_1, route_2, route_3, route_4, route_5, route_6,
       z_order
FROM osm_transportation_name_linestring_gen2
WHERE (highway = 'motorway' OR highway = 'construction' AND subclass = 'motorway')
  AND ST_Length(geometry) > 20000
;
CREATE TABLE IF NOT EXISTS osm_transportation_name_linestring_gen3 AS
SELECT *
FROM osm_transportation_name_linestring_gen3_view;
CREATE INDEX IF NOT EXISTS osm_transportation_name_linestring_gen3_name_ref_idx ON osm_transportation_name_linestring_gen3((coalesce(tags->'name', ref)));
CREATE INDEX IF NOT EXISTS osm_transportation_name_linestring_gen3_geometry_idx ON osm_transportation_name_linestring_gen3 USING gist (geometry);

CREATE INDEX IF NOT EXISTS osm_transportation_name_linestring_gen3_highway_partial_idx
    ON osm_transportation_name_linestring_gen3 (highway, subclass)
    WHERE highway IN ('motorway', 'construction');

-- etldoc: osm_transportation_name_linestring_gen3 -> osm_transportation_name_linestring_gen4
CREATE OR REPLACE VIEW osm_transportation_name_linestring_gen4_view AS
SELECT ST_Simplify(geometry, 500) AS geometry,
       tags,
       ref,
       highway,
       subclass,
       brunnel,
       network,
       route_1, route_2, route_3, route_4, route_5, route_6,
       z_order
FROM osm_transportation_name_linestring_gen3
WHERE (highway = 'motorway' OR highway = 'construction' AND subclass = 'motorway')
  AND ST_Length(geometry) > 20000
;
CREATE TABLE IF NOT EXISTS osm_transportation_name_linestring_gen4 AS
SELECT *
FROM osm_transportation_name_linestring_gen4_view;
CREATE INDEX IF NOT EXISTS osm_transportation_name_linestring_gen4_name_ref_idx ON osm_transportation_name_linestring_gen4((coalesce(tags->'name', ref)));
CREATE INDEX IF NOT EXISTS osm_transportation_name_linestring_gen4_geometry_idx ON osm_transportation_name_linestring_gen4 USING gist (geometry);

-- Handle updates

CREATE SCHEMA IF NOT EXISTS transportation_name;

-- Trigger to update "osm_transportation_name_network" from "osm_route_member" and "osm_highway_linestring"

CREATE TABLE IF NOT EXISTS transportation_name.network_changes
(
    osm_id bigint,
    UNIQUE (osm_id)
);

CREATE OR REPLACE FUNCTION transportation_name.route_member_store() RETURNS trigger AS
$$
BEGIN
    INSERT INTO transportation_name.network_changes(osm_id)
    VALUES (CASE WHEN tg_op IN ('DELETE', 'UPDATE') THEN old.member ELSE new.member END)
    ON CONFLICT(osm_id) DO NOTHING;

    RETURN NULL;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION transportation_name.highway_linestring_store() RETURNS trigger AS
$$
BEGIN
    INSERT INTO transportation_name.network_changes(osm_id)
    VALUES (CASE WHEN tg_op IN ('DELETE', 'UPDATE') THEN old.osm_id ELSE new.osm_id END)
    ON CONFLICT(osm_id) DO NOTHING;

    RETURN NULL;
END;
$$ LANGUAGE plpgsql;

CREATE TABLE IF NOT EXISTS transportation_name.updates_network
(
    id serial PRIMARY KEY,
    t text,
    UNIQUE (t)
);
CREATE OR REPLACE FUNCTION transportation_name.flag_network() RETURNS trigger AS
$$
BEGIN
    INSERT INTO transportation_name.updates_network(t) VALUES ('y') ON CONFLICT(t) DO NOTHING;
    RETURN NULL;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION transportation_name.refresh_network() RETURNS trigger AS
$$
DECLARE
    t TIMESTAMP WITH TIME ZONE := clock_timestamp();
BEGIN
    RAISE LOG 'Refresh transportation_name_network';
    PERFORM update_osm_route_member();

    -- REFRESH osm_transportation_name_network
    DELETE
    FROM osm_transportation_name_network AS n
        USING
            transportation_name.network_changes AS c
    WHERE n.osm_id = c.osm_id;

    UPDATE osm_highway_linestring hl
    SET network = rm.network_type
    FROM transportation_name.network_changes c,
         transportation_route_member_coalesced rm
    WHERE hl.osm_id=c.osm_id
      AND hl.osm_id=rm.member
      AND rm.concurrency_index=1;

    UPDATE osm_highway_linestring_gen_z11 hl
    SET network = rm.network_type
    FROM transportation_name.network_changes c,
         transportation_route_member_coalesced rm
    WHERE hl.osm_id=c.osm_id
      AND hl.osm_id=rm.member
      AND rm.concurrency_index=1;

    INSERT INTO osm_transportation_name_network
    SELECT
        geometry,
        osm_id,
        tags || get_basic_names(tags, geometry) AS tags,
        ref,
        highway,
        subclass,
        brunnel,
        level,
        sac_scale,
        layer,
        indoor,
        network_type,
        route_1, route_2, route_3, route_4, route_5, route_6,
        z_order,
        route_rank
    FROM (
        SELECT hl.geometry,
            hl.osm_id,
            transportation_name_tags(hl.geometry, hl.tags, hl.name, hl.name_en, hl.name_de) AS tags,
            rm1.network_type,
            CASE
                WHEN rm1.network_type IS NOT NULL AND rm1.ref::text <> ''
                    THEN rm1.ref::text
                ELSE NULLIF(hl.ref, '')
                END AS ref,
            hl.highway,
            NULLIF(hl.construction, '') AS subclass,
            brunnel(hl.is_bridge, hl.is_tunnel, hl.is_ford) AS brunnel,
            sac_scale,
            CASE WHEN highway IN ('footway', 'steps') THEN layer END AS layer,
            CASE WHEN highway IN ('footway', 'steps') THEN level END AS level,
            CASE WHEN highway IN ('footway', 'steps') THEN indoor END AS indoor,
	    NULLIF(rm1.network, '') || '=' || COALESCE(rm1.ref, '') AS route_1,
	    NULLIF(rm2.network, '') || '=' || COALESCE(rm2.ref, '') AS route_2,
	    NULLIF(rm3.network, '') || '=' || COALESCE(rm3.ref, '') AS route_3,
	    NULLIF(rm4.network, '') || '=' || COALESCE(rm4.ref, '') AS route_4,
	    NULLIF(rm5.network, '') || '=' || COALESCE(rm5.ref, '') AS route_5,
	    NULLIF(rm6.network, '') || '=' || COALESCE(rm6.ref, '') AS route_6,
            hl.z_order,
            LEAST(rm1.rank, rm2.rank, rm3.rank, rm4.rank, rm5.rank, rm6.rank) AS route_rank
        FROM osm_highway_linestring hl
                JOIN transportation_name.network_changes AS c ON
            hl.osm_id = c.osm_id
		LEFT OUTER JOIN transportation_route_member_coalesced rm1 ON rm1.member = hl.osm_id AND rm1.concurrency_index=1
		LEFT OUTER JOIN transportation_route_member_coalesced rm2 ON rm2.member = hl.osm_id AND rm2.concurrency_index=2
		LEFT OUTER JOIN transportation_route_member_coalesced rm3 ON rm3.member = hl.osm_id AND rm3.concurrency_index=3
		LEFT OUTER JOIN transportation_route_member_coalesced rm4 ON rm4.member = hl.osm_id AND rm4.concurrency_index=4
		LEFT OUTER JOIN transportation_route_member_coalesced rm5 ON rm5.member = hl.osm_id AND rm5.concurrency_index=5
		LEFT OUTER JOIN transportation_route_member_coalesced rm6 ON rm6.member = hl.osm_id AND rm6.concurrency_index=6
	WHERE (hl.name <> '' OR hl.ref <> '' OR rm1.ref <> '' OR rm1.network <> '')
          AND hl.highway <> ''
    ) AS t
    ON CONFLICT DO NOTHING;

    -- noinspection SqlWithoutWhere
    DELETE FROM transportation_name.network_changes;
    -- noinspection SqlWithoutWhere
    DELETE FROM transportation_name.updates_network;

    RAISE LOG 'Refresh transportation_name network done in %', age(clock_timestamp(), t);
    RETURN NULL;
END;
$$ LANGUAGE plpgsql;


CREATE TRIGGER trigger_store_transportation_route_member
    AFTER INSERT OR UPDATE OR DELETE
    ON osm_route_member
    FOR EACH ROW
EXECUTE PROCEDURE transportation_name.route_member_store();

CREATE TRIGGER trigger_store_transportation_highway_linestring
    AFTER INSERT OR UPDATE OR DELETE
    ON osm_highway_linestring
    FOR EACH ROW
EXECUTE PROCEDURE transportation_name.highway_linestring_store();

CREATE TRIGGER trigger_flag_transportation_name
    AFTER INSERT
    ON transportation_name.network_changes
    FOR EACH STATEMENT
EXECUTE PROCEDURE transportation_name.flag_network();

CREATE CONSTRAINT TRIGGER trigger_refresh_network
    AFTER INSERT
    ON transportation_name.updates_network
    INITIALLY DEFERRED
    FOR EACH ROW
EXECUTE PROCEDURE transportation_name.refresh_network();

-- Trigger to update "osm_transportation_name_linestring" from "osm_transportation_name_network"

CREATE TABLE IF NOT EXISTS transportation_name.name_changes
(
    id serial PRIMARY KEY,
    is_old boolean,
    osm_id bigint,
    tags hstore,
    ref character varying,
    highway character varying,
    subclass character varying,
    brunnel character varying,
    sac_scale character varying,
    level integer,
    layer integer,
    indoor boolean,
    network_type route_network_type,
    route_1 character varying,
    route_2 character varying,
    route_3 character varying,
    route_4 character varying,
    route_5 character varying,
    route_6 character varying
);

CREATE OR REPLACE FUNCTION transportation_name.name_network_store() RETURNS trigger AS
$$
BEGIN
    IF (tg_op IN ('DELETE', 'UPDATE'))
    THEN
        INSERT INTO transportation_name.name_changes(is_old, osm_id, tags, ref, highway, subclass,
                                                     brunnel, sac_scale, level, layer, indoor, network_type,
                                                     route_1, route_2, route_3, route_4, route_5, route_6)
        VALUES (TRUE, old.osm_id, old.tags, old.ref, old.highway, old.subclass,
                old.brunnel, old.sac_scale, old.level, old.layer, old.indoor, old.network_type,
                old.route_1, old.route_2, old.route_3, old.route_4, old.route_5, old.route_6);
    END IF;
    IF (tg_op IN ('UPDATE', 'INSERT'))
    THEN
        INSERT INTO transportation_name.name_changes(is_old, osm_id, tags, ref, highway, subclass,
                                                     brunnel, sac_scale, level, layer, indoor, network_type,
                                                     route_1, route_2, route_3, route_4, route_5, route_6)
        VALUES (FALSE, new.osm_id, new.tags, new.ref, new.highway, new.subclass,
                new.brunnel, new.sac_scale, new.level, new.layer, new.indoor, new.network_type,
                new.route_1, new.route_2, new.route_3, new.route_4, new.route_5, new.route_6);
    END IF;
    RETURN NULL;
END;
$$ LANGUAGE plpgsql;

CREATE TABLE IF NOT EXISTS transportation_name.updates_name
(
    id serial PRIMARY KEY,
    t  text,
    UNIQUE (t)
);
CREATE OR REPLACE FUNCTION transportation_name.flag_name() RETURNS trigger AS
$$
BEGIN
    INSERT INTO transportation_name.updates_name(t) VALUES ('y') ON CONFLICT(t) DO NOTHING;
    RETURN NULL;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION transportation_name.refresh_name() RETURNS trigger AS
$BODY$
DECLARE
    t TIMESTAMP WITH TIME ZONE := clock_timestamp();
BEGIN
    RAISE LOG 'Refresh transportation_name';

    -- REFRESH osm_transportation_name_linestring

    -- Compact the change history to keep only the first and last version, and then uniq version of row
    CREATE TEMP TABLE name_changes_compact AS
    SELECT DISTINCT ON (tags, ref, highway, subclass, brunnel, sac_scale, level, layer, indoor, network_type,
                        route_1, route_2, route_3, route_4, route_5, route_6)
        tags,
        ref,
        highway,
        subclass,
        brunnel,
        sac_scale,
        level,
        layer,
        indoor,
        network_type,
        route_1, route_2, route_3, route_4, route_5, route_6,
        coalesce(tags->'name', ref) AS name_ref
    FROM ((
              SELECT DISTINCT ON (osm_id) *
              FROM transportation_name.name_changes
              WHERE is_old
              ORDER BY osm_id,
                       id ASC
          )
          UNION ALL
          (
              SELECT DISTINCT ON (osm_id) *
              FROM transportation_name.name_changes
              WHERE NOT is_old
              ORDER BY osm_id,
                       id DESC
          )) AS t;

    DELETE
    FROM osm_transportation_name_linestring AS n
        USING name_changes_compact AS c
    WHERE coalesce(n.ref, '') = coalesce(c.ref, '')
      AND coalesce(n.tags, '') = coalesce(c.tags, '')
      AND n.highway IS NOT DISTINCT FROM c.highway
      AND n.subclass IS NOT DISTINCT FROM c.subclass
      AND n.brunnel IS NOT DISTINCT FROM c.brunnel
      AND n.sac_scale IS NOT DISTINCT FROM c.sac_scale
      AND n.level IS NOT DISTINCT FROM c.level
      AND n.layer IS NOT DISTINCT FROM c.layer
      AND n.indoor IS NOT DISTINCT FROM c.indoor
      AND n.network IS NOT DISTINCT FROM c.network_type
      AND n.route_1 IS NOT DISTINCT FROM c.route_1
      AND n.route_2 IS NOT DISTINCT FROM c.route_2
      AND n.route_3 IS NOT DISTINCT FROM c.route_3
      AND n.route_4 IS NOT DISTINCT FROM c.route_4
      AND n.route_5 IS NOT DISTINCT FROM c.route_5
      AND n.route_6 IS NOT DISTINCT FROM c.route_6;

    INSERT INTO osm_transportation_name_linestring
    SELECT (ST_Dump(geometry)).geom AS geometry,
           tags|| get_basic_names(tags, geometry) AS tags,
           ref,
           highway,
           subclass,
           brunnel,
           sac_scale,
           level,
           layer,
           indoor,
           network_type AS network,
           route_1, route_2, route_3, route_4, route_5, route_6,
           z_order
    FROM (
        SELECT ST_LineMerge(ST_Collect(n.geometry)) AS geometry,
            n.tags,
            n.ref,
            n.highway,
            n.subclass,
            n.brunnel,
            n.sac_scale,
            n.level,
            n.layer,
            n.indoor,
            n.network_type,
            n.route_1, n.route_2, n.route_3, n.route_4, n.route_5, n.route_6,
            min(n.z_order) AS z_order
        FROM osm_transportation_name_network AS n
            JOIN name_changes_compact AS c ON
                 coalesce(n.ref, '') = coalesce(c.ref, '')
             AND coalesce(n.tags, '') = coalesce(c.tags, '')
             AND n.highway IS NOT DISTINCT FROM c.highway
             AND n.subclass IS NOT DISTINCT FROM c.subclass
             AND n.brunnel IS NOT DISTINCT FROM c.brunnel
             AND n.sac_scale IS NOT DISTINCT FROM c.sac_scale
             AND n.level IS NOT DISTINCT FROM c.level
             AND n.layer IS NOT DISTINCT FROM c.layer
             AND n.indoor IS NOT DISTINCT FROM c.indoor
             AND n.network_type IS NOT DISTINCT FROM c.network_type
             AND n.route_1 IS NOT DISTINCT FROM c.route_1
             AND n.route_2 IS NOT DISTINCT FROM c.route_2
             AND n.route_3 IS NOT DISTINCT FROM c.route_3
             AND n.route_4 IS NOT DISTINCT FROM c.route_4
             AND n.route_5 IS NOT DISTINCT FROM c.route_5
             AND n.route_6 IS NOT DISTINCT FROM c.route_6
        GROUP BY n.tags, n.ref, n.highway, n.subclass, n.brunnel, n.sac_scale, n.level, n.layer, n.indoor, n.network_type,
                 n.route_1, n.route_2, n.route_3, n.route_4, n.route_5, n.route_6
    ) AS highway_union;

    -- REFRESH osm_transportation_name_linestring_gen1
    DELETE FROM osm_transportation_name_linestring_gen1 AS n
    USING name_changes_compact AS c
    WHERE
        coalesce(n.tags->'name', n.ref) = c.name_ref
        AND coalesce(n.tags, '') = coalesce(c.tags, '')
        AND n.ref IS NOT DISTINCT FROM c.ref
        AND n.highway IS NOT DISTINCT FROM c.highway
        AND n.subclass IS NOT DISTINCT FROM c.subclass
        AND n.brunnel IS NOT DISTINCT FROM c.brunnel
        AND n.network IS NOT DISTINCT FROM c.network_type
        AND n.route_1 IS NOT DISTINCT FROM c.route_1
        AND n.route_2 IS NOT DISTINCT FROM c.route_2
        AND n.route_3 IS NOT DISTINCT FROM c.route_3
        AND n.route_4 IS NOT DISTINCT FROM c.route_4
        AND n.route_5 IS NOT DISTINCT FROM c.route_5
        AND n.route_6 IS NOT DISTINCT FROM c.route_6;

    INSERT INTO osm_transportation_name_linestring_gen1
    SELECT n.*
    FROM osm_transportation_name_linestring_gen1_view AS n
        JOIN name_changes_compact AS c ON
            coalesce(n.tags->'name', n.ref) = c.name_ref
            AND coalesce(n.tags, '') = coalesce(c.tags, '')
            AND n.ref IS NOT DISTINCT FROM c.ref
            AND n.highway IS NOT DISTINCT FROM c.highway
            AND n.subclass IS NOT DISTINCT FROM c.subclass
            AND n.brunnel IS NOT DISTINCT FROM c.brunnel
            AND n.network IS NOT DISTINCT FROM c.network_type
            AND n.route_1 IS NOT DISTINCT FROM c.route_1
            AND n.route_2 IS NOT DISTINCT FROM c.route_2
            AND n.route_3 IS NOT DISTINCT FROM c.route_3
            AND n.route_4 IS NOT DISTINCT FROM c.route_4
            AND n.route_5 IS NOT DISTINCT FROM c.route_5
            AND n.route_6 IS NOT DISTINCT FROM c.route_6;

    -- REFRESH osm_transportation_name_linestring_gen2
    DELETE FROM osm_transportation_name_linestring_gen2 AS n
    USING name_changes_compact AS c
    WHERE
        coalesce(n.tags->'name', n.ref) = c.name_ref
        AND coalesce(n.tags, '') = coalesce(c.tags, '')
        AND n.ref IS NOT DISTINCT FROM c.ref
        AND n.highway IS NOT DISTINCT FROM c.highway
        AND n.subclass IS NOT DISTINCT FROM c.subclass
        AND n.brunnel IS NOT DISTINCT FROM c.brunnel
        AND n.network IS NOT DISTINCT FROM c.network_type
        AND n.route_1 IS NOT DISTINCT FROM c.route_1
        AND n.route_2 IS NOT DISTINCT FROM c.route_2
        AND n.route_3 IS NOT DISTINCT FROM c.route_3
        AND n.route_4 IS NOT DISTINCT FROM c.route_4
        AND n.route_5 IS NOT DISTINCT FROM c.route_5
        AND n.route_6 IS NOT DISTINCT FROM c.route_6;

    INSERT INTO osm_transportation_name_linestring_gen2
    SELECT n.*
    FROM osm_transportation_name_linestring_gen2_view AS n
        JOIN name_changes_compact AS c ON
            coalesce(n.tags->'name', n.ref) = c.name_ref
            AND coalesce(n.tags, '') = coalesce(c.tags, '')
            AND n.ref IS NOT DISTINCT FROM c.ref
            AND n.highway IS NOT DISTINCT FROM c.highway
            AND n.subclass IS NOT DISTINCT FROM c.subclass
            AND n.brunnel IS NOT DISTINCT FROM c.brunnel
            AND n.network IS NOT DISTINCT FROM c.network_type
            AND n.route_1 IS NOT DISTINCT FROM c.route_1
            AND n.route_2 IS NOT DISTINCT FROM c.route_2
            AND n.route_3 IS NOT DISTINCT FROM c.route_3
            AND n.route_4 IS NOT DISTINCT FROM c.route_4
            AND n.route_5 IS NOT DISTINCT FROM c.route_5
            AND n.route_6 IS NOT DISTINCT FROM c.route_6;

    -- REFRESH osm_transportation_name_linestring_gen3
    DELETE FROM osm_transportation_name_linestring_gen3 AS n
    USING name_changes_compact AS c
    WHERE
        coalesce(n.tags->'name', n.ref) = c.name_ref
        AND coalesce(n.tags, '') = coalesce(c.tags, '')
        AND n.ref IS NOT DISTINCT FROM c.ref
        AND n.highway IS NOT DISTINCT FROM c.highway
        AND n.subclass IS NOT DISTINCT FROM c.subclass
        AND n.brunnel IS NOT DISTINCT FROM c.brunnel
        AND n.network IS NOT DISTINCT FROM c.network_type
        AND n.route_1 IS NOT DISTINCT FROM c.route_1
        AND n.route_2 IS NOT DISTINCT FROM c.route_2
        AND n.route_3 IS NOT DISTINCT FROM c.route_3
        AND n.route_4 IS NOT DISTINCT FROM c.route_4
        AND n.route_5 IS NOT DISTINCT FROM c.route_5
        AND n.route_6 IS NOT DISTINCT FROM c.route_6;

    INSERT INTO osm_transportation_name_linestring_gen3
    SELECT n.*
    FROM osm_transportation_name_linestring_gen3_view AS n
        JOIN name_changes_compact AS c ON
            coalesce(n.tags->'name', n.ref) = c.name_ref
            AND coalesce(n.tags, '') = coalesce(c.tags, '')
            AND n.ref IS NOT DISTINCT FROM c.ref
            AND n.highway IS NOT DISTINCT FROM c.highway
            AND n.subclass IS NOT DISTINCT FROM c.subclass
            AND n.brunnel IS NOT DISTINCT FROM c.brunnel
            AND n.network IS NOT DISTINCT FROM c.network_type
            AND n.route_1 IS NOT DISTINCT FROM c.route_1
            AND n.route_2 IS NOT DISTINCT FROM c.route_2
            AND n.route_3 IS NOT DISTINCT FROM c.route_3
            AND n.route_4 IS NOT DISTINCT FROM c.route_4
            AND n.route_5 IS NOT DISTINCT FROM c.route_5
            AND n.route_6 IS NOT DISTINCT FROM c.route_6;

    -- REFRESH osm_transportation_name_linestring_gen4
    DELETE FROM osm_transportation_name_linestring_gen4 AS n
    USING name_changes_compact AS c
    WHERE
        coalesce(n.tags->'name', n.ref) = c.name_ref
        AND coalesce(n.tags, '') = coalesce(c.tags, '')
        AND n.ref IS NOT DISTINCT FROM c.ref
        AND n.highway IS NOT DISTINCT FROM c.highway
        AND n.subclass IS NOT DISTINCT FROM c.subclass
        AND n.brunnel IS NOT DISTINCT FROM c.brunnel
        AND n.network IS NOT DISTINCT FROM c.network_type
        AND n.route_1 IS NOT DISTINCT FROM c.route_1
        AND n.route_2 IS NOT DISTINCT FROM c.route_2
        AND n.route_3 IS NOT DISTINCT FROM c.route_3
        AND n.route_4 IS NOT DISTINCT FROM c.route_4
        AND n.route_5 IS NOT DISTINCT FROM c.route_5
        AND n.route_6 IS NOT DISTINCT FROM c.route_6;

    INSERT INTO osm_transportation_name_linestring_gen4
    SELECT n.*
    FROM osm_transportation_name_linestring_gen4_view AS n
        JOIN name_changes_compact AS c ON
            coalesce(n.tags->'name', n.ref) = c.name_ref
            AND coalesce(n.tags, '') = coalesce(c.tags, '')
            AND n.ref IS NOT DISTINCT FROM c.ref
            AND n.highway IS NOT DISTINCT FROM c.highway
            AND n.subclass IS NOT DISTINCT FROM c.subclass
            AND n.brunnel IS NOT DISTINCT FROM c.brunnel
            AND n.network IS NOT DISTINCT FROM c.network_type
            AND n.route_1 IS NOT DISTINCT FROM c.route_1
            AND n.route_2 IS NOT DISTINCT FROM c.route_2
            AND n.route_3 IS NOT DISTINCT FROM c.route_3
            AND n.route_4 IS NOT DISTINCT FROM c.route_4
            AND n.route_5 IS NOT DISTINCT FROM c.route_5
            AND n.route_6 IS NOT DISTINCT FROM c.route_6;

    DROP TABLE name_changes_compact;
    DELETE FROM transportation_name.name_changes;
    DELETE FROM transportation_name.updates_name;

    RAISE LOG 'Refresh transportation_name done in %', age(clock_timestamp(), t);
    RETURN NULL;
END;
$BODY$
    LANGUAGE plpgsql;


CREATE TRIGGER trigger_store_transportation_name_network
    AFTER INSERT OR UPDATE OR DELETE
    ON osm_transportation_name_network
    FOR EACH ROW
EXECUTE PROCEDURE transportation_name.name_network_store();

CREATE TRIGGER trigger_flag_name
    AFTER INSERT
    ON transportation_name.name_changes
    FOR EACH STATEMENT
EXECUTE PROCEDURE transportation_name.flag_name();

CREATE CONSTRAINT TRIGGER trigger_refresh_name
    AFTER INSERT
    ON transportation_name.updates_name
    INITIALLY DEFERRED
    FOR EACH ROW
EXECUTE PROCEDURE transportation_name.refresh_name();

-- Layer transportation_name - ./transportation_name.sql

-- etldoc: layer_transportation_name[shape=record fillcolor=lightpink, style="rounded,filled",
-- etldoc:     label="layer_transportation_name | <z6> z6 | <z7> z7 | <z8> z8 |<z9> z9 |<z10> z10 |<z11> z11 |<z12> z12|<z13> z13|<z14_> z14+" ] ;

CREATE OR REPLACE FUNCTION layer_transportation_name(bbox geometry, zoom_level integer)
    RETURNS TABLE
            (
                geometry   geometry,
                name       text,
                name_en    text,
                name_de    text,
                tags       hstore,
                ref        text,
                ref_length int,
                network    text,
                route_1    text,
                route_2    text,
                route_3    text,
                route_4    text,
                route_5    text,
                route_6    text,
                class      text,
                subclass   text,
                brunnel    text,
                layer      int,
                level      int,
                indoor     int
            )
AS
$$
SELECT geometry,
       tags->'name' AS name,
       COALESCE(tags->'name:en', tags->'name') AS name_en,
       COALESCE(tags->'name:de', tags->'name', tags->'name:en') AS name_de,
       tags,
       ref,
       NULLIF(LENGTH(ref), 0) AS ref_length,
       CASE
           WHEN network IS NOT NULL
               THEN network::text
           WHEN length(coalesce(ref, '')) > 0
               THEN 'road'
           END AS network,
       route_1, route_2, route_3, route_4, route_5, route_6,
       highway_class(highway, '', subclass) AS class,
       CASE
           WHEN highway IS NOT NULL AND highway_class(highway, '', subclass) = 'path'
               THEN highway
           ELSE subclass
           END AS subclass,
       brunnel,
       NULLIF(layer, 0) AS layer,
       "level",
       CASE WHEN indoor = TRUE THEN 1 END AS indoor
FROM (

         -- etldoc: osm_transportation_name_linestring_gen4 ->  layer_transportation_name:z6
         SELECT *,
                NULL::int AS layer,
                NULL::int AS level,
                NULL::boolean AS indoor
         FROM osm_transportation_name_linestring_gen4
         WHERE zoom_level = 6
         UNION ALL

         -- etldoc: osm_transportation_name_linestring_gen3 ->  layer_transportation_name:z7
         SELECT *,
                NULL::int AS layer,
                NULL::int AS level,
                NULL::boolean AS indoor
         FROM osm_transportation_name_linestring_gen3
         WHERE zoom_level = 7
         UNION ALL

         -- etldoc: osm_transportation_name_linestring_gen2 ->  layer_transportation_name:z8
         SELECT *,
                NULL::int AS layer,
                NULL::int AS level,
                NULL::boolean AS indoor
         FROM osm_transportation_name_linestring_gen2
         WHERE zoom_level = 8
         UNION ALL

         -- etldoc: osm_transportation_name_linestring_gen1 ->  layer_transportation_name:z9
         -- etldoc: osm_transportation_name_linestring_gen1 ->  layer_transportation_name:z10
         -- etldoc: osm_transportation_name_linestring_gen1 ->  layer_transportation_name:z11
         SELECT *,
                NULL::int AS layer,
                NULL::int AS level,
                NULL::boolean AS indoor
         FROM osm_transportation_name_linestring_gen1
         WHERE zoom_level BETWEEN 9 AND 11
         UNION ALL

         -- etldoc: osm_transportation_name_linestring ->  layer_transportation_name:z12
         SELECT geometry,
                "tags",
                ref,
                highway,
                subclass,
                brunnel,
                network,
                route_1, route_2, route_3, route_4, route_5, route_6,
                z_order,
                layer,
                "level",
                indoor
         FROM osm_transportation_name_linestring
         WHERE zoom_level = 12
           AND LineLabel(zoom_level, COALESCE(tags->'name', ref), geometry)
           AND NOT highway_is_link(highway)
           AND
               CASE WHEN highway_class(highway, NULL::text, NULL::text) NOT IN ('path', 'minor') THEN TRUE
                    WHEN highway IN ('aerialway', 'unclassified', 'residential', 'shipway') THEN TRUE
                    WHEN route_rank = 1 THEN TRUE END

         UNION ALL

         -- etldoc: osm_transportation_name_linestring ->  layer_transportation_name:z13
         SELECT geometry,
                "tags",
                ref,
                highway,
                subclass,
                brunnel,
                network,
                route_1, route_2, route_3, route_4, route_5, route_6,
                z_order,
                layer,
                "level",
                indoor
         FROM osm_transportation_name_linestring
         WHERE zoom_level = 13
           AND LineLabel(zoom_level, COALESCE(tags->'name', ref), geometry)
           AND
               CASE WHEN highway <> 'path' THEN TRUE
                    WHEN highway = 'path' AND (
                                                   tags->'name' <> ''
                                                OR network IS NOT NULL
                                                OR sac_scale <> ''
                                                OR route_rank <= 2
                                              ) THEN TRUE
               END

         UNION ALL

         -- etldoc: osm_transportation_name_linestring ->  layer_transportation_name:z14_
         SELECT geometry,
                "tags",
                ref,
                highway,
                subclass,
                brunnel,
                network,
                route_1, route_2, route_3, route_4, route_5, route_6,
                z_order,
                layer,
                "level",
                indoor
         FROM osm_transportation_name_linestring
         WHERE zoom_level >= 14
         UNION ALL

         -- etldoc: osm_highway_point ->  layer_transportation_name:z10
         SELECT
		p.geometry,
                p.tags,
                p.ref,
                (
                  SELECT highest_highway(l.tags->'highway')
                    FROM osm_highway_linestring l
                    WHERE ST_Intersects(p.geometry,l.geometry)
                ) AS class,
                'junction'::text AS subclass,
                NULL AS brunnel,
                NULL AS network,
                NULL::text AS route_1,
                NULL::text AS route_2,
                NULL::text AS route_3,
                NULL::text AS route_4,
                NULL::text AS route_5,
                NULL::text AS route_6,
                z_order,
                layer,
                NULL::int AS level,
                NULL::boolean AS indoor
         FROM osm_highway_point p
         WHERE highway = 'motorway_junction' AND zoom_level >= 10
     ) AS zoom_levels
WHERE geometry && bbox
ORDER BY z_order ASC;
$$ LANGUAGE SQL STABLE
                -- STRICT
                PARALLEL SAFE;

DO $$ BEGIN RAISE NOTICE 'Finished layer transportation_name'; END$$;
