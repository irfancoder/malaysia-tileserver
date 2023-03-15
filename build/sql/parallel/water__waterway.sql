DO $$ BEGIN RAISE NOTICE 'Processing layer water'; END$$;

DO $$ BEGIN
    PERFORM 'ne_10m_lakes'::regclass;
EXCEPTION
    WHEN undefined_table THEN
        RAISE EXCEPTION '%', SQLERRM
            USING DETAIL = 'this table or view is required for layer "water"';
END;
$$ LANGUAGE 'plpgsql';

DO $$ BEGIN
    PERFORM 'ne_10m_ocean'::regclass;
EXCEPTION
    WHEN undefined_table THEN
        RAISE EXCEPTION '%', SQLERRM
            USING DETAIL = 'this table or view is required for layer "water"';
END;
$$ LANGUAGE 'plpgsql';

DO $$ BEGIN
    PERFORM 'ne_110m_lakes'::regclass;
EXCEPTION
    WHEN undefined_table THEN
        RAISE EXCEPTION '%', SQLERRM
            USING DETAIL = 'this table or view is required for layer "water"';
END;
$$ LANGUAGE 'plpgsql';

DO $$ BEGIN
    PERFORM 'ne_110m_ocean'::regclass;
EXCEPTION
    WHEN undefined_table THEN
        RAISE EXCEPTION '%', SQLERRM
            USING DETAIL = 'this table or view is required for layer "water"';
END;
$$ LANGUAGE 'plpgsql';

DO $$ BEGIN
    PERFORM 'ne_50m_lakes'::regclass;
EXCEPTION
    WHEN undefined_table THEN
        RAISE EXCEPTION '%', SQLERRM
            USING DETAIL = 'this table or view is required for layer "water"';
END;
$$ LANGUAGE 'plpgsql';

DO $$ BEGIN
    PERFORM 'ne_50m_ocean'::regclass;
EXCEPTION
    WHEN undefined_table THEN
        RAISE EXCEPTION '%', SQLERRM
            USING DETAIL = 'this table or view is required for layer "water"';
END;
$$ LANGUAGE 'plpgsql';

DO $$ BEGIN
    PERFORM 'osm_ocean_polygon'::regclass;
EXCEPTION
    WHEN undefined_table THEN
        RAISE EXCEPTION '%', SQLERRM
            USING DETAIL = 'this table or view is required for layer "water"';
END;
$$ LANGUAGE 'plpgsql';

-- Layer water - ./update_water.sql

-- Recreate ocean layer by union regular squares into larger polygons
-- etldoc: osm_ocean_polygon -> osm_ocean_polygon_union
CREATE TABLE IF NOT EXISTS osm_ocean_polygon_union AS
    (
    SELECT (ST_Dump(ST_Union(ST_MakeValid(geometry)))).geom::geometry(Polygon, 3857) AS geometry 
    FROM osm_ocean_polygon
    --for union select just full square (not big triangles)
    WHERE ST_Area(geometry) > 100000000 AND 
          ST_NPoints(geometry) = 5
    UNION ALL
    SELECT geometry 
    FROM osm_ocean_polygon
    -- as 321 records have less then 5 coordinates (triangle)
    -- bigger then 5 coordinates have squares with holes from island and coastline
    WHERE ST_NPoints(geometry) <> 5
    );

CREATE INDEX IF NOT EXISTS osm_ocean_polygon_union_geom_idx
  ON osm_ocean_polygon_union
  USING GIST (geometry);

--Drop data from original table but keep table as `CREATE TABLE IF NOT EXISTS` still test if query is valid
TRUNCATE TABLE osm_ocean_polygon;

-- This statement can be deleted after the water importer image stops creating this object as a table
DO
$$
    BEGIN
        DROP TABLE IF EXISTS osm_ocean_polygon_gen_z11 CASCADE;
    EXCEPTION
        WHEN wrong_object_type THEN
    END;
$$ LANGUAGE plpgsql;

-- etldoc: osm_ocean_polygon_union -> osm_ocean_polygon_gen_z11
DROP MATERIALIZED VIEW IF EXISTS osm_ocean_polygon_gen_z11 CASCADE;
CREATE MATERIALIZED VIEW osm_ocean_polygon_gen_z11 AS
(
SELECT ST_Simplify(geometry, ZRes(13)) AS geometry
FROM osm_ocean_polygon_union
    ) /* DELAY_MATERIALIZED_VIEW_CREATION */ ;
CREATE INDEX IF NOT EXISTS osm_ocean_polygon_gen_z11_idx ON osm_ocean_polygon_gen_z11 USING gist (geometry);


-- This statement can be deleted after the water importer image stops creating this object as a table
DO
$$
    BEGIN
        DROP TABLE IF EXISTS osm_ocean_polygon_gen_z10 CASCADE;
    EXCEPTION
        WHEN wrong_object_type THEN
    END;
$$ LANGUAGE plpgsql;

-- etldoc: osm_ocean_polygon_gen_z11 -> osm_ocean_polygon_gen_z10
DROP MATERIALIZED VIEW IF EXISTS osm_ocean_polygon_gen_z10 CASCADE;
CREATE MATERIALIZED VIEW osm_ocean_polygon_gen_z10 AS
(
SELECT ST_Simplify(geometry, ZRes(12)) AS geometry
FROM osm_ocean_polygon_gen_z11
    ) /* DELAY_MATERIALIZED_VIEW_CREATION */ ;
CREATE INDEX IF NOT EXISTS osm_ocean_polygon_gen_z10_idx ON osm_ocean_polygon_gen_z10 USING gist (geometry);


-- This statement can be deleted after the water importer image stops creating this object as a table
DO
$$
    BEGIN
        DROP TABLE IF EXISTS osm_ocean_polygon_gen_z9 CASCADE;
    EXCEPTION
        WHEN wrong_object_type THEN
    END;
$$ LANGUAGE plpgsql;

-- etldoc: osm_ocean_polygon_gen_z10 -> osm_ocean_polygon_gen_z9
DROP MATERIALIZED VIEW IF EXISTS osm_ocean_polygon_gen_z9 CASCADE;
CREATE MATERIALIZED VIEW osm_ocean_polygon_gen_z9 AS
(
SELECT ST_Simplify(geometry, ZRes(11)) AS geometry
FROM osm_ocean_polygon_gen_z10
    ) /* DELAY_MATERIALIZED_VIEW_CREATION */ ;
CREATE INDEX IF NOT EXISTS osm_ocean_polygon_gen_z9_idx ON osm_ocean_polygon_gen_z9 USING gist (geometry);


-- This statement can be deleted after the water importer image stops creating this object as a table
DO
$$
    BEGIN
        DROP TABLE IF EXISTS osm_ocean_polygon_gen_z8 CASCADE;
    EXCEPTION
        WHEN wrong_object_type THEN
    END;
$$ LANGUAGE plpgsql;

-- etldoc: osm_ocean_polygon_gen_z9 -> osm_ocean_polygon_gen_z8
DROP MATERIALIZED VIEW IF EXISTS osm_ocean_polygon_gen_z8 CASCADE;
CREATE MATERIALIZED VIEW osm_ocean_polygon_gen_z8 AS
(
SELECT ST_Simplify(geometry, ZRes(10)) AS geometry
FROM osm_ocean_polygon_gen_z9
    ) /* DELAY_MATERIALIZED_VIEW_CREATION */ ;
CREATE INDEX IF NOT EXISTS osm_ocean_polygon_gen_z8_idx ON osm_ocean_polygon_gen_z8 USING gist (geometry);


-- This statement can be deleted after the water importer image stops creating this object as a table
DO
$$
    BEGIN
        DROP TABLE IF EXISTS osm_ocean_polygon_gen_z7 CASCADE;
    EXCEPTION
        WHEN wrong_object_type THEN
    END;
$$ LANGUAGE plpgsql;

-- etldoc: osm_ocean_polygon_gen_z8 -> osm_ocean_polygon_gen_z7
DROP MATERIALIZED VIEW IF EXISTS osm_ocean_polygon_gen_z7 CASCADE;
CREATE MATERIALIZED VIEW osm_ocean_polygon_gen_z7 AS
(
SELECT ST_Simplify(geometry, ZRes(9)) AS geometry
FROM osm_ocean_polygon_gen_z8
    ) /* DELAY_MATERIALIZED_VIEW_CREATION */ ;
CREATE INDEX IF NOT EXISTS osm_ocean_polygon_gen_z7_idx ON osm_ocean_polygon_gen_z7 USING gist (geometry);


-- This statement can be deleted after the water importer image stops creating this object as a table
DO
$$
    BEGIN
        DROP TABLE IF EXISTS osm_ocean_polygon_gen_z6 CASCADE;
    EXCEPTION
        WHEN wrong_object_type THEN
    END;
$$ LANGUAGE plpgsql;

-- etldoc: osm_ocean_polygon_gen_z7 -> osm_ocean_polygon_gen_z6
DROP MATERIALIZED VIEW IF EXISTS osm_ocean_polygon_gen_z6 CASCADE;
CREATE MATERIALIZED VIEW osm_ocean_polygon_gen_z6 AS
(
SELECT ST_Simplify(geometry, ZRes(8)) AS geometry
FROM osm_ocean_polygon_gen_z7
    ) /* DELAY_MATERIALIZED_VIEW_CREATION */ ;
CREATE INDEX IF NOT EXISTS osm_ocean_polygon_gen_z6_idx ON osm_ocean_polygon_gen_z6 USING gist (geometry);

-- Layer water - ./water.sql

CREATE OR REPLACE FUNCTION water_class(waterway text, water text, leisure text) RETURNS text AS
$$
SELECT CASE
           WHEN water IN ('river', 'canal', 'stream', 'ditch', 'drain') THEN 'river'
           WHEN "waterway" = 'dock' THEN 'dock'
           WHEN "water" IN ('river', 'stream', 'canal', 'ditch', 'drain') THEN 'river'
           WHEN "water" IN ('pond', 'basin', 'wastewater') THEN 'pond'
           WHEN "leisure" = 'swimming_pool' THEN 'swimming_pool'
           ELSE 'lake'
           END;
$$ LANGUAGE SQL IMMUTABLE
                PARALLEL SAFE;


CREATE OR REPLACE FUNCTION waterway_brunnel(is_bridge bool, is_tunnel bool) RETURNS text AS
$$
SELECT CASE
           WHEN is_bridge THEN 'bridge'
           WHEN is_tunnel THEN 'tunnel'
           END;
$$ LANGUAGE SQL IMMUTABLE
                STRICT
                PARALLEL SAFE;


-- Get matching osm id for natural earth id.
DROP MATERIALIZED VIEW IF EXISTS match_osm_ne_id CASCADE;
CREATE MATERIALIZED VIEW match_osm_ne_id AS
(
WITH name_match AS
    (
        -- Distinct on keeps just the first occurence -> order by 'area_ratio DESC'.
    SELECT DISTINCT ON (ne.ne_id) 
        ne.ne_id,
        osm.osm_id,
        (ST_Area(ST_Intersection(ne.geometry, osm.geometry))/ST_Area(ne.geometry)) AS area_ratio
    FROM ne_10m_lakes ne, osm_water_polygon_gen_z6 osm
    WHERE ne.name = osm.name 
        AND ST_Intersects(ne.geometry, osm.geometry)
    ORDER BY ne_id,
             area_ratio DESC
    ),
        -- Add lakes which are not match by name, but intersects. 
        -- Duplicity solves 'DISTICT ON' with 'area_ratio'.
    geom_match AS
    (SELECT DISTINCT ON (ne.ne_id) 
        ne.ne_id,
        osm.osm_id,
        (ST_Area(ST_Intersection(ne.geometry, osm.geometry))/ST_Area(ne.geometry)) AS area_ratio
	FROM ne_10m_lakes ne, osm_water_polygon_gen_z6 osm
	WHERE ST_Intersects(ne.geometry, osm.geometry)
        AND ne.ne_id NOT IN 
            (   SELECT ne_id 
                FROM name_match
            )
    ORDER BY ne_id,
             area_ratio DESC
    )
 
SELECT  ne_id,
        osm_id 
FROM name_match

UNION

SELECT  ne_id,
        osm_id 
FROM geom_match
);

-- ne_10m_ocean
-- etldoc:  ne_10m_ocean ->  ne_10m_ocean_gen_z5
DROP MATERIALIZED VIEW IF EXISTS ne_10m_ocean_gen_z5 CASCADE;
CREATE MATERIALIZED VIEW ne_10m_ocean_gen_z5 AS
(
SELECT  NULL::integer AS id,
       (ST_Dump(ST_Simplify(geometry, ZRes(7)))).geom AS geometry,
       'ocean'::text AS class,
       NULL::boolean AS is_intermittent,
       NULL::boolean AS is_bridge,
       NULL::boolean AS is_tunnel
FROM ne_10m_ocean
    ) /* DELAY_MATERIALIZED_VIEW_CREATION */ ;
CREATE INDEX IF NOT EXISTS ne_10m_ocean_gen_z5_idx ON ne_10m_ocean_gen_z5 USING gist (geometry);

-- ne_10m_lakes
-- etldoc:  ne_10m_lakes ->  ne_10m_lakes_gen_z5
DROP MATERIALIZED VIEW IF EXISTS ne_10m_lakes_gen_z5 CASCADE;
CREATE MATERIALIZED VIEW ne_10m_lakes_gen_z5 AS
(
SELECT COALESCE(osm.osm_id, ne_id) AS id,
        -- Union fixing e.g. Lake Huron and Georgian Bay duplicity 
       (ST_Dump(ST_MakeValid(ST_Simplify(ST_Union(geometry), ZRes(7))))).geom AS geometry,
       'lake'::text AS class,
       NULL::boolean AS is_intermittent,
       NULL::boolean AS is_bridge,
       NULL::boolean AS is_tunnel
FROM ne_10m_lakes
LEFT JOIN match_osm_ne_id osm USING (ne_id)
GROUP BY COALESCE(osm.osm_id, ne_id), is_intermittent, is_bridge, is_tunnel
    ) /* DELAY_MATERIALIZED_VIEW_CREATION */ ;
CREATE INDEX IF NOT EXISTS ne_10m_lakes_gen_z5_idx ON ne_10m_lakes_gen_z5 USING gist (geometry);

-- etldoc:  ne_10m_lakes_gen_z5 ->  ne_10m_lakes_gen_z4
DROP MATERIALIZED VIEW IF EXISTS ne_10m_lakes_gen_z4 CASCADE;
CREATE MATERIALIZED VIEW ne_10m_lakes_gen_z4 AS
(
SELECT id,
       (ST_Dump(ST_MakeValid(ST_Simplify(geometry, ZRes(6))))).geom AS geometry,
       class,
       is_intermittent,
       is_bridge,
       is_tunnel
FROM ne_10m_lakes_gen_z5
    ) /* DELAY_MATERIALIZED_VIEW_CREATION */ ;
CREATE INDEX IF NOT EXISTS ne_10m_lakes_gen_z4_idx ON ne_10m_lakes_gen_z4 USING gist (geometry);

-- ne_50m_ocean
-- etldoc:  ne_50m_ocean ->  ne_50m_ocean_gen_z4
DROP MATERIALIZED VIEW IF EXISTS ne_50m_ocean_gen_z4 CASCADE;
CREATE MATERIALIZED VIEW ne_50m_ocean_gen_z4 AS
(
SELECT NULL::integer AS id,
       (ST_Dump(ST_Simplify(geometry, ZRes(6)))).geom AS geometry,
       'ocean'::text AS class,
       NULL::boolean AS is_intermittent,
       NULL::boolean AS is_bridge,
       NULL::boolean AS is_tunnel
FROM ne_50m_ocean
    ) /* DELAY_MATERIALIZED_VIEW_CREATION */ ;
CREATE INDEX IF NOT EXISTS ne_50m_ocean_gen_z4_idx ON ne_50m_ocean_gen_z4 USING gist (geometry);

-- etldoc:  ne_50m_ocean_gen_z4 ->  ne_50m_ocean_gen_z3
DROP MATERIALIZED VIEW IF EXISTS ne_50m_ocean_gen_z3 CASCADE;
CREATE MATERIALIZED VIEW ne_50m_ocean_gen_z3 AS
(
SELECT id,
       ST_Simplify(geometry, ZRes(5)) AS geometry,
       class,
       is_intermittent,
       is_bridge,
       is_tunnel
FROM ne_50m_ocean_gen_z4
    ) /* DELAY_MATERIALIZED_VIEW_CREATION */ ;
CREATE INDEX IF NOT EXISTS ne_50m_ocean_gen_z3_idx ON ne_50m_ocean_gen_z3 USING gist (geometry);

-- etldoc:  ne_50m_ocean_gen_z3 ->  ne_50m_ocean_gen_z2
DROP MATERIALIZED VIEW IF EXISTS ne_50m_ocean_gen_z2 CASCADE;
CREATE MATERIALIZED VIEW ne_50m_ocean_gen_z2 AS
(
SELECT id,
       ST_Simplify(geometry, ZRes(4)) AS geometry,
       class,
       is_intermittent,
       is_bridge,
       is_tunnel
FROM ne_50m_ocean_gen_z3
    ) /* DELAY_MATERIALIZED_VIEW_CREATION */ ;
CREATE INDEX IF NOT EXISTS ne_50m_ocean_gen_z2_idx ON ne_50m_ocean_gen_z2 USING gist (geometry);

-- ne_50m_lakes
-- etldoc:  ne_50m_lakes ->  ne_50m_lakes_gen_z3
DROP MATERIALIZED VIEW IF EXISTS ne_50m_lakes_gen_z3 CASCADE;
CREATE MATERIALIZED VIEW ne_50m_lakes_gen_z3 AS
(
SELECT COALESCE(osm.osm_id, ne_id) AS id,
       (ST_Dump(ST_MakeValid(ST_Simplify(geometry, ZRes(5))))).geom AS geometry,
       'lake'::text AS class,
       NULL::boolean AS is_intermittent,
       NULL::boolean AS is_bridge,
       NULL::boolean AS is_tunnel
FROM ne_50m_lakes
LEFT JOIN match_osm_ne_id osm USING (ne_id)
    ) /* DELAY_MATERIALIZED_VIEW_CREATION */ ;
CREATE INDEX IF NOT EXISTS ne_50m_lakes_gen_z3_idx ON ne_50m_lakes_gen_z3 USING gist (geometry);

-- etldoc:  ne_50m_lakes_gen_z3 ->  ne_50m_lakes_gen_z2
DROP MATERIALIZED VIEW IF EXISTS ne_50m_lakes_gen_z2 CASCADE;
CREATE MATERIALIZED VIEW ne_50m_lakes_gen_z2 AS
(
SELECT id,
       (ST_Dump(ST_MakeValid(ST_Simplify(geometry, ZRes(4))))).geom AS geometry,
       class,
       is_intermittent,
       is_bridge,
       is_tunnel
FROM ne_50m_lakes_gen_z3
    ) /* DELAY_MATERIALIZED_VIEW_CREATION */ ;
CREATE INDEX IF NOT EXISTS ne_50m_lakes_gen_z2_idx ON ne_50m_lakes_gen_z2 USING gist (geometry);

--ne_110m_ocean
-- etldoc:  ne_110m_ocean ->  ne_110m_ocean_gen_z1
DROP MATERIALIZED VIEW IF EXISTS ne_110m_ocean_gen_z1 CASCADE;
CREATE MATERIALIZED VIEW ne_110m_ocean_gen_z1 AS
(
SELECT NULL::integer AS id,
       ST_Simplify(geometry, ZRes(3)) AS geometry,
       'ocean'::text AS class,
       NULL::boolean AS is_intermittent,
       NULL::boolean AS is_bridge,
       NULL::boolean AS is_tunnel
FROM ne_110m_ocean
    ) /* DELAY_MATERIALIZED_VIEW_CREATION */ ;
CREATE INDEX IF NOT EXISTS ne_110m_ocean_gen_z1_idx ON ne_110m_ocean_gen_z1 USING gist (geometry);

-- etldoc:  ne_110m_ocean_gen_z1 ->  ne_110m_ocean_gen_z0
DROP MATERIALIZED VIEW IF EXISTS ne_110m_ocean_gen_z0 CASCADE;
CREATE MATERIALIZED VIEW ne_110m_ocean_gen_z0 AS
(
SELECT id,
       ST_Simplify(geometry, ZRes(2)) AS geometry,
       class,
       is_intermittent,
       is_bridge,
       is_tunnel
FROM ne_110m_ocean_gen_z1
    ) /* DELAY_MATERIALIZED_VIEW_CREATION */ ;
CREATE INDEX IF NOT EXISTS ne_110m_ocean_gen_z0_idx ON ne_110m_ocean_gen_z0 USING gist (geometry);


-- ne_110m_lakes
-- etldoc:  ne_110m_lakes ->  ne_110m_lakes_gen_z1
DROP MATERIALIZED VIEW IF EXISTS ne_110m_lakes_gen_z1 CASCADE;
CREATE MATERIALIZED VIEW ne_110m_lakes_gen_z1 AS
(
SELECT COALESCE(osm.osm_id, ne_id) AS id,
       (ST_Dump(ST_Simplify(geometry, ZRes(3)))).geom AS geometry,
       'lake'::text AS class,
       NULL::boolean AS is_intermittent,
       NULL::boolean AS is_bridge,
       NULL::boolean AS is_tunnel
FROM ne_110m_lakes
LEFT JOIN match_osm_ne_id osm USING (ne_id)
    ) /* DELAY_MATERIALIZED_VIEW_CREATION */ ;
CREATE INDEX IF NOT EXISTS ne_110m_lakes_gen_z1_idx ON ne_110m_lakes_gen_z1 USING gist (geometry);

-- etldoc:  ne_110m_lakes_gen_z1 ->  ne_110m_lakes_gen_z0
DROP MATERIALIZED VIEW IF EXISTS ne_110m_lakes_gen_z0 CASCADE;
CREATE MATERIALIZED VIEW ne_110m_lakes_gen_z0 AS
(
SELECT id,
       (ST_Dump(ST_Simplify(geometry, ZRes(2)))).geom AS geometry,
       class,
       is_intermittent,
       is_bridge,
       is_tunnel
FROM ne_110m_lakes_gen_z1
    ) /* DELAY_MATERIALIZED_VIEW_CREATION */ ;
CREATE INDEX IF NOT EXISTS ne_110m_lakes_gen_z0_idx ON ne_110m_lakes_gen_z0 USING gist (geometry);

DROP MATERIALIZED VIEW IF EXISTS water_z6;
DROP MATERIALIZED VIEW IF EXISTS water_z7;
DROP MATERIALIZED VIEW IF EXISTS water_z8;
DROP MATERIALIZED VIEW IF EXISTS water_z9;
DROP MATERIALIZED VIEW IF EXISTS water_z10;
DROP MATERIALIZED VIEW IF EXISTS water_z11;
DROP MATERIALIZED VIEW IF EXISTS water_z12;

CREATE OR REPLACE VIEW water_z0 AS
(
-- etldoc:  ne_110m_ocean_gen_z0 ->  water_z0
SELECT id,
       geometry,
       class,
       is_intermittent,
       is_bridge,
       is_tunnel
FROM ne_110m_ocean_gen_z0
UNION ALL
-- etldoc:  ne_110m_lakes_gen_z0 ->  water_z0
SELECT id,
       geometry,
       class,
       is_intermittent,
       is_bridge,
       is_tunnel
FROM ne_110m_lakes_gen_z0
    );

CREATE OR REPLACE VIEW water_z1 AS
(
-- etldoc:  ne_110m_ocean_gen_z1 ->  water_z1
SELECT id,
       geometry,
       class,
       is_intermittent,
       is_bridge,
       is_tunnel
FROM ne_110m_ocean_gen_z1
UNION ALL
-- etldoc:  ne_110m_lakes_gen_z1 ->  water_z1
SELECT id,
       geometry,
       class,
       is_intermittent,
       is_bridge,
       is_tunnel
FROM ne_110m_lakes_gen_z1
    );

CREATE OR REPLACE VIEW water_z2 AS
(
-- etldoc:  ne_50m_ocean_gen_z2 ->  water_z2
SELECT id,
       geometry,
       class,
       is_intermittent,
       is_bridge,
       is_tunnel
FROM ne_50m_ocean_gen_z2
UNION ALL
-- etldoc:  ne_50m_lakes_gen_z2 ->  water_z2
SELECT id,
       geometry,
       class,
       is_intermittent,
       is_bridge,
       is_tunnel
FROM ne_50m_lakes_gen_z2
    );

CREATE OR REPLACE VIEW water_z3 AS
(
-- etldoc:  ne_50m_ocean_gen_z3 ->  water_z3
SELECT id,
       geometry,
       class,
       is_intermittent,
       is_bridge,
       is_tunnel
FROM ne_50m_ocean_gen_z3
UNION ALL
-- etldoc:  ne_50m_lakes_gen_z3 ->  water_z3
SELECT id, 
       geometry,
       class,
       is_intermittent,
       is_bridge,
       is_tunnel
FROM ne_50m_lakes_gen_z3
    );

CREATE OR REPLACE VIEW water_z4 AS
(
-- etldoc:  ne_50m_ocean_gen_z4 ->  water_z4
SELECT id,
       geometry,
       class,
       is_intermittent,
       is_bridge,
       is_tunnel
FROM ne_50m_ocean_gen_z4
UNION ALL
-- etldoc:  ne_10m_lakes_gen_z4 ->  water_z4
SELECT id,
       geometry,
       class,
       is_intermittent,
       is_bridge,
       is_tunnel
FROM ne_10m_lakes_gen_z4
    );


CREATE OR REPLACE VIEW water_z5 AS
(
-- etldoc:  ne_10m_ocean_gen_z5 ->  water_z5
SELECT id,
       geometry,
       class,
       is_intermittent,
       is_bridge,
       is_tunnel
FROM ne_10m_ocean_gen_z5
UNION ALL
-- etldoc:  ne_10m_lakes_gen_z5 ->  water_z5
SELECT id,
       geometry,
       class,
       is_intermittent,
       is_bridge,
       is_tunnel
FROM ne_10m_lakes_gen_z5
    );

CREATE MATERIALIZED VIEW water_z6 AS
(
-- etldoc:  osm_ocean_polygon_gen_z6 ->  water_z6
SELECT NULL::integer AS id,
       (ST_Dump(geometry)).geom AS geometry,
       'ocean'::text AS class,
       NULL::boolean AS is_intermittent,
       NULL::boolean AS is_bridge,
       NULL::boolean AS is_tunnel
FROM osm_ocean_polygon_gen_z6
UNION ALL
-- etldoc:  osm_water_polygon_gen_z6 ->  water_z6
SELECT osm_id AS id,
       (ST_Dump(geometry)).geom AS geometry,
       water_class(waterway, water, leisure) AS class,
       is_intermittent,
       NULL::boolean AS is_bridge,
       NULL::boolean AS is_tunnel
FROM osm_water_polygon_gen_z6
WHERE "natural" != 'bay'
    );
CREATE INDEX ON water_z6 USING gist(geometry);

CREATE MATERIALIZED VIEW water_z7 AS
(
-- etldoc:  osm_ocean_polygon_gen_z7 ->  water_z7
SELECT NULL::integer AS id,
       (ST_Dump(geometry)).geom AS geometry,
       'ocean'::text AS class,
       NULL::boolean AS is_intermittent,
       NULL::boolean AS is_bridge,
       NULL::boolean AS is_tunnel
FROM osm_ocean_polygon_gen_z7
UNION ALL
-- etldoc:  osm_water_polygon_gen_z7 ->  water_z7
SELECT osm_id AS id,
       (ST_Dump(geometry)).geom AS geometry,
       water_class(waterway, water, leisure) AS class,
       is_intermittent,
       NULL::boolean AS is_bridge,
       NULL::boolean AS is_tunnel
FROM osm_water_polygon_gen_z7
WHERE "natural" != 'bay'
    );
CREATE INDEX ON water_z7 USING gist(geometry);

CREATE MATERIALIZED VIEW water_z8 AS
(
-- etldoc:  osm_ocean_polygon_gen_z8 ->  water_z8
SELECT NULL::integer AS id,
       (ST_Dump(geometry)).geom AS geometry,
       'ocean'::text AS class,
       NULL::boolean AS is_intermittent,
       NULL::boolean AS is_bridge,
       NULL::boolean AS is_tunnel
FROM osm_ocean_polygon_gen_z8
UNION ALL
-- etldoc:  osm_water_polygon_gen_z8 ->  water_z8
SELECT osm_id AS id,
       (ST_Dump(geometry)).geom AS geometry,
       water_class(waterway, water, leisure) AS class,
       is_intermittent,
       NULL::boolean AS is_bridge,
       NULL::boolean AS is_tunnel
FROM osm_water_polygon_gen_z8
WHERE "natural" != 'bay'
    );
CREATE INDEX ON water_z8 USING gist(geometry);

CREATE MATERIALIZED VIEW water_z9 AS
(
-- etldoc:  osm_ocean_polygon_gen_z9 ->  water_z9
SELECT NULL::integer AS id,
       (ST_Dump(geometry)).geom AS geometry,
       'ocean'::text AS class,
       NULL::boolean AS is_intermittent,
       NULL::boolean AS is_bridge,
       NULL::boolean AS is_tunnel
FROM osm_ocean_polygon_gen_z9
UNION ALL
-- etldoc:  osm_water_polygon_gen_z9 ->  water_z9
SELECT osm_id AS id,
       (ST_Dump(geometry)).geom AS geometry,
       water_class(waterway, water, leisure) AS class,
       is_intermittent,
       NULL::boolean AS is_bridge,
       NULL::boolean AS is_tunnel
FROM osm_water_polygon_gen_z9
WHERE "natural" != 'bay'
    );
CREATE INDEX ON water_z9 USING gist(geometry);

CREATE MATERIALIZED VIEW water_z10 AS
(
-- etldoc:  osm_ocean_polygon_gen_z10 ->  water_z10
SELECT NULL::integer AS id,
       (ST_Dump(geometry)).geom AS geometry,
       'ocean'::text AS class,
       NULL::boolean AS is_intermittent,
       NULL::boolean AS is_bridge,
       NULL::boolean AS is_tunnel
FROM osm_ocean_polygon_gen_z10
UNION ALL
-- etldoc:  osm_water_polygon_gen_z10 ->  water_z10
SELECT osm_id AS id,
       (ST_Dump(geometry)).geom AS geometry,
       water_class(waterway, water, leisure) AS class,
       is_intermittent,
       NULL::boolean AS is_bridge,
       NULL::boolean AS is_tunnel
FROM osm_water_polygon_gen_z10
WHERE "natural" != 'bay'
    );
CREATE INDEX ON water_z10 USING gist(geometry);

CREATE MATERIALIZED VIEW water_z11 AS
(
-- etldoc:  osm_ocean_polygon_gen_z11 ->  water_z11
SELECT NULL::integer AS id,
       (ST_Dump(geometry)).geom AS geometry,
       'ocean'::text AS class,
       NULL::boolean AS is_intermittent,
       NULL::boolean AS is_bridge,
       NULL::boolean AS is_tunnel
FROM osm_ocean_polygon_gen_z11
UNION ALL
-- etldoc:  osm_water_polygon_gen_z11 ->  water_z11
SELECT osm_id AS id,
       (ST_Dump(geometry)).geom AS geometry,
       water_class(waterway, water, leisure) AS class,
       is_intermittent,
       NULL::boolean AS is_bridge,
       NULL::boolean AS is_tunnel
FROM osm_water_polygon_gen_z11
WHERE "natural" != 'bay'
    );
CREATE INDEX ON water_z11 USING gist(geometry);

CREATE MATERIALIZED VIEW water_z12 AS
(
-- etldoc:  osm_ocean_polygon_union ->  water_z12
SELECT NULL::integer AS id,
       (ST_Dump(geometry)).geom AS geometry,
       'ocean'::text AS class,
       NULL::boolean AS is_intermittent,
       NULL::boolean AS is_bridge,
       NULL::boolean AS is_tunnel
FROM osm_ocean_polygon_union
UNION ALL
-- etldoc:  osm_water_polygon ->  water_z12
SELECT osm_id AS id,
       (ST_Dump(geometry)).geom AS geometry,
       water_class(waterway, water, leisure) AS class,
       is_intermittent,
       is_bridge,
       is_tunnel
FROM osm_water_polygon
WHERE "natural" != 'bay'
    );
CREATE INDEX ON water_z12 USING gist(geometry);

-- etldoc: layer_water [shape=record fillcolor=lightpink, style="rounded,filled",
-- etldoc:     label="layer_water |<z0> z0|<z1>z1|<z2>z2|<z3>z3 |<z4> z4|<z5>z5|<z6>z6|<z7>z7| <z8> z8 |<z9> z9 |<z10> z10 |<z11> z11 |<z12> z12+" ] ;

CREATE OR REPLACE FUNCTION layer_water(bbox geometry, zoom_level int)
    RETURNS TABLE
            (
                id           bigint,
                geometry     geometry,
                class        text,
                brunnel      text,
                intermittent int
            )
AS
$$
SELECT id,
       geometry,
       class::text,
       waterway_brunnel(is_bridge, is_tunnel) AS brunnel,
       is_intermittent::int AS intermittent
FROM (
         -- etldoc: water_z0 ->  layer_water:z0
         SELECT *
         FROM water_z0
         WHERE zoom_level = 0
         UNION ALL
         -- etldoc: water_z1 ->  layer_water:z1
         SELECT *
         FROM water_z1
         WHERE zoom_level = 1
         UNION ALL
         -- etldoc: water_z2 ->  layer_water:z2
         SELECT *
         FROM water_z2
         WHERE zoom_level = 2
         UNION ALL
         -- etldoc: water_z3 ->  layer_water:z3
         SELECT *
         FROM water_z3
         WHERE zoom_level = 3
         UNION ALL
         -- etldoc: water_z4 ->  layer_water:z4
         SELECT *
         FROM water_z4
         WHERE zoom_level = 4
         UNION ALL
         -- etldoc: water_z5 ->  layer_water:z5
         SELECT *
         FROM water_z5
         WHERE zoom_level = 5
         UNION ALL
         -- etldoc: water_z6 ->  layer_water:z6
         SELECT *
         FROM water_z6
         WHERE zoom_level = 6
         UNION ALL
         -- etldoc: water_z7 ->  layer_water:z7
         SELECT *
         FROM water_z7
         WHERE zoom_level = 7
         UNION ALL
         -- etldoc: water_z8 ->  layer_water:z8
         SELECT *
         FROM water_z8
         WHERE zoom_level = 8
         UNION ALL
         -- etldoc: water_z9 ->  layer_water:z9
         SELECT *
         FROM water_z9
         WHERE zoom_level = 9
         UNION ALL
         -- etldoc: water_z10 ->  layer_water:z10
         SELECT *
         FROM water_z10
         WHERE zoom_level = 10
         UNION ALL
         -- etldoc: water_z11 ->  layer_water:z11
         SELECT *
         FROM water_z11
         WHERE zoom_level = 11
         UNION ALL
         -- etldoc: water_z12 ->  layer_water:z12
         SELECT *
         FROM water_z12
         WHERE zoom_level >= 12
     ) AS zoom_levels
WHERE geometry && bbox;
$$ LANGUAGE SQL STABLE
                -- STRICT
                PARALLEL SAFE;

DO $$ BEGIN RAISE NOTICE 'Finished layer water'; END$$;

DO $$ BEGIN RAISE NOTICE 'Processing layer waterway'; END$$;

DO $$ BEGIN
    PERFORM 'ne_110m_rivers_lake_centerlines'::regclass;
EXCEPTION
    WHEN undefined_table THEN
        RAISE EXCEPTION '%', SQLERRM
            USING DETAIL = 'this table or view is required for layer "waterway"';
END;
$$ LANGUAGE 'plpgsql';

DO $$ BEGIN
    PERFORM 'ne_50m_rivers_lake_centerlines'::regclass;
EXCEPTION
    WHEN undefined_table THEN
        RAISE EXCEPTION '%', SQLERRM
            USING DETAIL = 'this table or view is required for layer "waterway"';
END;
$$ LANGUAGE 'plpgsql';

-- Layer waterway - ./update_waterway_linestring.sql

DROP TRIGGER IF EXISTS trigger_flag ON osm_waterway_linestring;
DROP TRIGGER IF EXISTS trigger_refresh ON osm_waterway_linestring;

DO
$$
    BEGIN
        UPDATE osm_waterway_linestring
        SET tags = update_tags(tags, geometry);
    END
$$;


-- Handle updates

CREATE SCHEMA IF NOT EXISTS waterway_linestring;
CREATE OR REPLACE FUNCTION waterway_linestring.refresh() RETURNS trigger AS
$$
BEGIN
    --     RAISE NOTICE 'Refresh waterway_linestring %', NEW.osm_id;
    NEW.tags = update_tags(NEW.tags, NEW.geometry);
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER trigger_refresh
    BEFORE INSERT OR UPDATE
    ON osm_waterway_linestring
    FOR EACH ROW
EXECUTE PROCEDURE waterway_linestring.refresh();

-- Layer waterway - ./update_important_waterway.sql

DROP TRIGGER IF EXISTS trigger_important_waterway_linestring ON osm_important_waterway_linestring;
DROP TRIGGER IF EXISTS trigger_store ON osm_waterway_linestring;
DROP TRIGGER IF EXISTS trigger_flag ON osm_waterway_linestring;
DROP TRIGGER IF EXISTS trigger_refresh ON waterway_important.updates;

-- We merge the waterways by name like the highways
-- This helps to drop not important rivers (since they do not have a name)
-- and also makes it possible to filter out too short rivers

CREATE INDEX IF NOT EXISTS osm_waterway_linestring_waterway_partial_idx
    ON osm_waterway_linestring ((true))
    WHERE name <> ''
      AND waterway = 'river'
      AND ST_IsValid(geometry);

CREATE TABLE IF NOT EXISTS osm_important_waterway_linestring (
    id SERIAL PRIMARY KEY,
    geometry geometry,
    name varchar,
    name_en varchar,
    name_de varchar,
    tags hstore
);

-- etldoc: osm_waterway_linestring ->  osm_important_waterway_linestring
INSERT INTO osm_important_waterway_linestring (geometry, name, name_en, name_de, tags)
SELECT (ST_Dump(geometry)).geom AS geometry,
       name,
       name_en,
       name_de,
       tags
FROM (
         SELECT ST_LineMerge(ST_Union(geometry)) AS geometry,
                name,
                name_en,
                name_de,
                slice_language_tags(tags) AS tags
         FROM osm_waterway_linestring
         WHERE name <> ''
           AND waterway = 'river'
           AND ST_IsValid(geometry)
         GROUP BY name, name_en, name_de, slice_language_tags(tags)
     ) AS waterway_union;
CREATE INDEX IF NOT EXISTS osm_important_waterway_linestring_geometry_idx ON osm_important_waterway_linestring USING gist (geometry);

CREATE TABLE IF NOT EXISTS osm_important_waterway_linestring_gen_z11
(LIKE osm_important_waterway_linestring);

CREATE TABLE IF NOT EXISTS osm_important_waterway_linestring_gen_z10
(LIKE osm_important_waterway_linestring_gen_z11);

CREATE TABLE IF NOT EXISTS osm_important_waterway_linestring_gen_z9
(LIKE osm_important_waterway_linestring_gen_z10);

CREATE OR REPLACE FUNCTION insert_important_waterway_linestring_gen(update_id bigint) RETURNS void AS
$$
BEGIN
    -- etldoc: osm_important_waterway_linestring -> osm_important_waterway_linestring_gen_z11
    INSERT INTO osm_important_waterway_linestring_gen_z11 (geometry, id, name, name_en, name_de, tags)
    SELECT ST_Simplify(geometry, ZRes(12)) AS geometry,
        id,
        name,
        name_en,
        name_de,
        tags
    FROM osm_important_waterway_linestring
    WHERE
        (update_id IS NULL OR id = update_id) AND
        ST_Length(geometry) > 1000;

    -- etldoc: osm_important_waterway_linestring_gen_z11 -> osm_important_waterway_linestring_gen_z10
    INSERT INTO osm_important_waterway_linestring_gen_z10 (geometry, id, name, name_en, name_de, tags)
    SELECT ST_Simplify(geometry, ZRes(11)) AS geometry,
        id,
        name,
        name_en,
        name_de,
        tags
    FROM osm_important_waterway_linestring_gen_z11
    WHERE
        (update_id IS NULL OR id = update_id) AND
        ST_Length(geometry) > 4000;

    -- etldoc: osm_important_waterway_linestring_gen_z10 -> osm_important_waterway_linestring_gen_z9
    INSERT INTO osm_important_waterway_linestring_gen_z9 (geometry, id, name, name_en, name_de, tags)
    SELECT ST_Simplify(geometry, ZRes(10)) AS geometry,
        id,
        name,
        name_en,
        name_de,
        tags
    FROM osm_important_waterway_linestring_gen_z10
    WHERE
        (update_id IS NULL OR id = update_id) AND
        ST_Length(geometry) > 8000;
END;
$$ LANGUAGE plpgsql;

TRUNCATE osm_important_waterway_linestring_gen_z11;
TRUNCATE osm_important_waterway_linestring_gen_z10;
TRUNCATE osm_important_waterway_linestring_gen_z9;
SELECT insert_important_waterway_linestring_gen(NULL);

CREATE INDEX IF NOT EXISTS osm_important_waterway_linestring_gen_z11_geometry_idx
    ON osm_important_waterway_linestring_gen_z11 USING gist (geometry);
CREATE INDEX IF NOT EXISTS osm_important_waterway_linestring_gen_z10_geometry_idx
    ON osm_important_waterway_linestring_gen_z10 USING gist (geometry);
CREATE INDEX IF NOT EXISTS osm_important_waterway_linestring_gen_z9_geometry_idx
    ON osm_important_waterway_linestring_gen_z9 USING gist (geometry);


-- Handle updates

CREATE SCHEMA IF NOT EXISTS waterway_important;

CREATE TABLE IF NOT EXISTS waterway_important.changes
(
    id serial PRIMARY KEY,
    osm_id bigint,
    is_old boolean,
    name character varying,
    name_en character varying,
    name_de character varying,
    tags hstore
);
CREATE OR REPLACE FUNCTION waterway_important.store() RETURNS trigger AS
$$
BEGIN
    IF (tg_op IN ('DELETE', 'UPDATE')) AND OLD.name <> '' AND OLD.waterway = 'river' THEN
        INSERT INTO waterway_important.changes(is_old, name, name_en, name_de, tags)
        VALUES (TRUE, OLD.name, OLD.name_en, OLD.name_de, slice_language_tags(OLD.tags));
    END IF;
    IF (tg_op IN ('UPDATE', 'INSERT')) AND NEW.name <> '' AND NEW.waterway = 'river' THEN
        INSERT INTO waterway_important.changes(is_old, name, name_en, name_de, tags)
        VALUES (FALSE, NEW.name, NEW.name_en, NEW.name_de, slice_language_tags(NEW.tags));
    END IF;
    RETURN NULL;
END;
$$ LANGUAGE plpgsql;

CREATE TABLE IF NOT EXISTS waterway_important.updates
(
    id serial PRIMARY KEY,
    t text,
    UNIQUE (t)
);
CREATE OR REPLACE FUNCTION waterway_important.flag() RETURNS trigger AS
$$
BEGIN
    INSERT INTO waterway_important.updates(t) VALUES ('y') ON CONFLICT(t) DO NOTHING;
    RETURN NULL;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION waterway_important.refresh() RETURNS trigger AS
$$
DECLARE
    t TIMESTAMP WITH TIME ZONE := clock_timestamp();
BEGIN
    RAISE LOG 'Refresh waterway';

    -- REFRESH osm_important_waterway_linestring

    -- Compact the change history to keep only the first and last version, and then uniq version of row
    CREATE TEMP TABLE changes_compact AS
    SELECT DISTINCT ON (name, name_en, name_de, tags)
        name,
        name_en,
        name_de,
        tags
    FROM ((
              SELECT DISTINCT ON (osm_id) *
              FROM waterway_important.changes
              WHERE is_old
              ORDER BY osm_id,
                       id ASC
          )
          UNION ALL
          (
              SELECT DISTINCT ON (osm_id) *
              FROM waterway_important.changes
              WHERE NOT is_old
              ORDER BY osm_id,
                       id DESC
          )) AS t;

    DELETE
    FROM osm_important_waterway_linestring AS w
        USING changes_compact AS c
    WHERE w.name = c.name
      AND w.name_en IS NOT DISTINCT FROM c.name_en
      AND w.name_de IS NOT DISTINCT FROM c.name_de
      AND w.tags IS NOT DISTINCT FROM c.tags;

    INSERT INTO osm_important_waterway_linestring (geometry, name, name_en, name_de, tags)
    SELECT (ST_Dump(geometry)).geom AS geometry,
           name,
           name_en,
           name_de,
           tags
    FROM (
             SELECT ST_LineMerge(ST_Union(geometry)) AS geometry,
                    w.name,
                    w.name_en,
                    w.name_de,
                    slice_language_tags(w.tags) AS tags
             FROM osm_waterway_linestring AS w
                      JOIN changes_compact AS c ON
                     w.name = c.name AND w.name_en IS NOT DISTINCT FROM c.name_en AND
                     w.name_de IS NOT DISTINCT FROM c.name_de AND
                     slice_language_tags(w.tags) IS NOT DISTINCT FROM c.tags
             WHERE w.name <> ''
               AND w.waterway = 'river'
               AND ST_IsValid(geometry)
             GROUP BY w.name, w.name_en, w.name_de, slice_language_tags(w.tags)
         ) AS waterway_union;

    DROP TABLE changes_compact;
    -- noinspection SqlWithoutWhere
    DELETE FROM waterway_important.changes;
    -- noinspection SqlWithoutWhere
    DELETE FROM waterway_important.updates;

    RAISE LOG 'Refresh waterway done in %', age(clock_timestamp(), t);
    RETURN NULL;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION waterway_important.important_waterway_linestring_gen_refresh() RETURNS trigger AS
$$
BEGIN
    IF (tg_op = 'DELETE' OR tg_op = 'UPDATE') THEN
        DELETE FROM osm_important_waterway_linestring_gen_z11 WHERE id = old.id;
        DELETE FROM osm_important_waterway_linestring_gen_z10 WHERE id = old.id;
        DELETE FROM osm_important_waterway_linestring_gen_z9 WHERE id = old.id;
    END IF;

    IF (tg_op = 'UPDATE' OR tg_op = 'INSERT') THEN
        PERFORM insert_important_waterway_linestring_gen(new.id);
    END IF;

    RETURN NULL;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER trigger_important_waterway_linestring
    AFTER INSERT OR UPDATE OR DELETE
    ON osm_important_waterway_linestring
    FOR EACH ROW
EXECUTE PROCEDURE waterway_important.important_waterway_linestring_gen_refresh();

CREATE TRIGGER trigger_store
    AFTER INSERT OR UPDATE OR DELETE
    ON osm_waterway_linestring
    FOR EACH ROW
EXECUTE PROCEDURE waterway_important.store();

CREATE TRIGGER trigger_flag
    AFTER INSERT OR UPDATE OR DELETE
    ON osm_waterway_linestring
    FOR EACH STATEMENT
EXECUTE PROCEDURE waterway_important.flag();

CREATE CONSTRAINT TRIGGER trigger_refresh
    AFTER INSERT
    ON waterway_important.updates
    INITIALLY DEFERRED
    FOR EACH ROW
EXECUTE PROCEDURE waterway_important.refresh();

-- Layer waterway - ./waterway.sql

CREATE OR REPLACE FUNCTION waterway_brunnel(is_bridge bool, is_tunnel bool) RETURNS text AS
$$
SELECT CASE
           WHEN is_bridge THEN 'bridge'
           WHEN is_tunnel THEN 'tunnel'
           END;
$$ LANGUAGE SQL IMMUTABLE
                STRICT
                PARALLEL SAFE;
-- ne_110m_rivers_lake_centerlines
-- etldoc: ne_110m_rivers_lake_centerlines ->  ne_110m_rivers_lake_centerlines_gen_z3
DROP MATERIALIZED VIEW IF EXISTS ne_110m_rivers_lake_centerlines_gen_z3 CASCADE;
CREATE MATERIALIZED VIEW ne_110m_rivers_lake_centerlines_gen_z3 AS
(
SELECT ST_Simplify(geometry, ZRes(5)) as geometry,
       'river'::text AS class,
       NULL::text AS name,
       NULL::text AS name_en,
       NULL::text AS name_de,
       NULL::hstore AS tags,
       NULL::boolean AS is_bridge,
       NULL::boolean AS is_tunnel,
       NULL::boolean AS is_intermittent
FROM ne_110m_rivers_lake_centerlines
WHERE featurecla = 'River'
    ) /* DELAY_MATERIALIZED_VIEW_CREATION */ ;
CREATE INDEX IF NOT EXISTS ne_110m_rivers_lake_centerlines_gen_z3_idx ON ne_110m_rivers_lake_centerlines_gen_z3 USING gist (geometry);

-- ne_50m_rivers_lake_centerlines
-- etldoc: ne_50m_rivers_lake_centerlines ->  ne_50m_rivers_lake_centerlines_gen_z5
DROP MATERIALIZED VIEW IF EXISTS ne_50m_rivers_lake_centerlines_gen_z5 CASCADE;
CREATE MATERIALIZED VIEW ne_50m_rivers_lake_centerlines_gen_z5 AS
(
SELECT ST_Simplify(geometry, ZRes(7)) as geometry,
       'river'::text AS class,
       NULL::text AS name,
       NULL::text AS name_en,
       NULL::text AS name_de,
       NULL::hstore AS tags,
       NULL::boolean AS is_bridge,
       NULL::boolean AS is_tunnel,
       NULL::boolean AS is_intermittent
FROM ne_50m_rivers_lake_centerlines
WHERE featurecla = 'River'
    ) /* DELAY_MATERIALIZED_VIEW_CREATION */ ;
CREATE INDEX IF NOT EXISTS ne_50m_rivers_lake_centerlines_gen_z5_idx ON ne_50m_rivers_lake_centerlines_gen_z5 USING gist (geometry);

-- etldoc: ne_50m_rivers_lake_centerlines_gen_z5 ->  ne_50m_rivers_lake_centerlines_gen_z4
DROP MATERIALIZED VIEW IF EXISTS ne_50m_rivers_lake_centerlines_gen_z4 CASCADE;
CREATE MATERIALIZED VIEW ne_50m_rivers_lake_centerlines_gen_z4 AS
(
SELECT ST_Simplify(geometry, ZRes(6)) as geometry,
       class,
       name,
       name_en,
       name_de,
       tags,
       is_bridge,
       is_tunnel,
       is_intermittent
FROM ne_50m_rivers_lake_centerlines_gen_z5
    ) /* DELAY_MATERIALIZED_VIEW_CREATION */ ;
CREATE INDEX IF NOT EXISTS ne_50m_rivers_lake_centerlines_gen_z4_idx ON ne_50m_rivers_lake_centerlines_gen_z4 USING gist (geometry);

-- osm_waterway_relation
-- etldoc: osm_waterway_relation -> waterway_relation
DROP TABLE IF EXISTS waterway_relation CASCADE;
CREATE TABLE waterway_relation AS (
    SELECT ST_Union(geometry) AS geometry,
           name,
           slice_language_tags(tags) AS tags
    FROM osm_waterway_relation
    WHERE name <> ''
      AND (role = 'main_stream' OR role = '')
      AND ST_GeometryType(geometry) = 'ST_LineString'
      AND ST_IsClosed(geometry) = FALSE
    GROUP BY name, slice_language_tags(tags)
);
CREATE INDEX IF NOT EXISTS waterway_relation_geometry_idx ON waterway_relation USING gist (geometry);

-- etldoc: waterway_relation -> waterway_relation_gen_z8
DROP MATERIALIZED VIEW IF EXISTS waterway_relation_gen_z8 CASCADE;
CREATE MATERIALIZED VIEW waterway_relation_gen_z8 AS (
    SELECT ST_Simplify(geometry, ZRes(10)) AS geometry,
       'river'::text AS class,
       name,
       NULL::text AS name_en,
       NULL::text AS name_de,
       tags,
       NULL::boolean AS is_bridge,
       NULL::boolean AS is_tunnel,
       NULL::boolean AS is_intermittent
    FROM waterway_relation
    WHERE ST_Length(geometry) > 300000
) /* DELAY_MATERIALIZED_VIEW_CREATION */ ;
CREATE INDEX IF NOT EXISTS waterway_relation_gen_z8_geometry_idx ON waterway_relation_gen_z8 USING gist (geometry);

-- etldoc: waterway_relation_gen_z8 -> waterway_relation_gen_z7
DROP MATERIALIZED VIEW IF EXISTS waterway_relation_gen_z7 CASCADE;
CREATE MATERIALIZED VIEW waterway_relation_gen_z7 AS (
    SELECT ST_Simplify(geometry, ZRes(9)) AS geometry,
       class,
       name,
       name_en,
       name_de,
       tags,
       is_bridge,
       is_tunnel,
       is_intermittent
    FROM waterway_relation_gen_z8
    WHERE ST_Length(geometry) > 400000
) /* DELAY_MATERIALIZED_VIEW_CREATION */ ;
CREATE INDEX IF NOT EXISTS waterway_relation_gen_z7_geometry_idx ON waterway_relation_gen_z7 USING gist (geometry);

-- etldoc: waterway_relation_gen_z7 -> waterway_relation_gen_z6
DROP MATERIALIZED VIEW IF EXISTS waterway_relation_gen_z6 CASCADE;
CREATE MATERIALIZED VIEW waterway_relation_gen_z6 AS (
    SELECT ST_Simplify(geometry, ZRes(8)) AS geometry,
       class,
       name,
       name_en,
       name_de,
       tags,
       is_bridge,
       is_tunnel,
       is_intermittent
    FROM waterway_relation_gen_z7
    WHERE ST_Length(geometry) > 500000
) /* DELAY_MATERIALIZED_VIEW_CREATION */ ;
CREATE INDEX IF NOT EXISTS waterway_relation_gen_z6_geometry_idx ON waterway_relation_gen_z6 USING gist (geometry);


-- etldoc: ne_110m_rivers_lake_centerlines_gen_z3 ->  waterway_z3
CREATE OR REPLACE VIEW waterway_z3 AS
(
SELECT geometry,
       class,
       name,
       name_en,
       name_de,
       tags,
       is_bridge,
       is_tunnel,
       is_intermittent
FROM ne_110m_rivers_lake_centerlines_gen_z3
    );

-- etldoc: ne_50m_rivers_lake_centerlines_gen_z4 ->  waterway_z4
CREATE OR REPLACE VIEW waterway_z4 AS
(
SELECT geometry,
       class,
       name,
       name_en,
       name_de,
       tags,
       is_bridge,
       is_tunnel,
       is_intermittent
FROM ne_50m_rivers_lake_centerlines_gen_z4
    );

-- etldoc: ne_50m_rivers_lake_centerlines_gen_z5 ->  waterway_z5
CREATE OR REPLACE VIEW waterway_z5 AS
(
SELECT geometry,
       class,
       name,
       name_en,
       name_de,
       tags,
       is_bridge,
       is_tunnel,
       is_intermittent
FROM ne_50m_rivers_lake_centerlines_gen_z5
    );

-- etldoc: waterway_relation_gen_z6 ->  waterway_z6
CREATE OR REPLACE VIEW waterway_z6 AS
(
SELECT geometry,
       class,
       name,
       name_en,
       name_de,
       tags,
       is_bridge,
       is_tunnel,
       is_intermittent
FROM waterway_relation_gen_z6
    );

-- etldoc: waterway_relation_gen_z7 ->  waterway_z7
CREATE OR REPLACE VIEW waterway_z7 AS
(
SELECT geometry,
       class,
       name,
       name_en,
       name_de,
       tags,
       is_bridge,
       is_tunnel,
       is_intermittent
FROM waterway_relation_gen_z7
    );

-- etldoc: waterway_relation_gen_z8 ->  waterway_z8
CREATE OR REPLACE VIEW waterway_z8 AS
(
SELECT geometry,
       class,
       name,
       name_en,
       name_de,
       tags,
       is_bridge,
       is_tunnel,
       is_intermittent
FROM waterway_relation_gen_z8
    );

-- etldoc: osm_important_waterway_linestring_gen_z9 ->  waterway_z9
CREATE OR REPLACE VIEW waterway_z9 AS
(
SELECT geometry,
       'river'::text AS class,
       name,
       name_en,
       name_de,
       tags,
       NULL::boolean AS is_bridge,
       NULL::boolean AS is_tunnel,
       NULL::boolean AS is_intermittent
FROM osm_important_waterway_linestring_gen_z9
    );

-- etldoc: osm_important_waterway_linestring_gen_z10 ->  waterway_z10
CREATE OR REPLACE VIEW waterway_z10 AS
(
SELECT geometry,
       'river'::text AS class,
       name,
       name_en,
       name_de,
       tags,
       NULL::boolean AS is_bridge,
       NULL::boolean AS is_tunnel,
       NULL::boolean AS is_intermittent
FROM osm_important_waterway_linestring_gen_z10
    );

-- etldoc:osm_important_waterway_linestring_gen_z11 ->  waterway_z11
CREATE OR REPLACE VIEW waterway_z11 AS
(
SELECT geometry,
       'river'::text AS class,
       name,
       name_en,
       name_de,
       tags,
       NULL::boolean AS is_bridge,
       NULL::boolean AS is_tunnel,
       NULL::boolean AS is_intermittent
FROM osm_important_waterway_linestring_gen_z11
    );

-- etldoc: osm_waterway_linestring ->  waterway_z12
CREATE OR REPLACE VIEW waterway_z12 AS
(
SELECT geometry,
       waterway::text AS class,
       name,
       name_en,
       name_de,
       tags,
       is_bridge,
       is_tunnel,
       is_intermittent
FROM osm_waterway_linestring
WHERE waterway IN ('river', 'canal')
    );

-- etldoc: osm_waterway_linestring ->  waterway_z13
CREATE OR REPLACE VIEW waterway_z13 AS
(
SELECT geometry,
       waterway::text AS class,
       name,
       name_en,
       name_de,
       tags,
       is_bridge,
       is_tunnel,
       is_intermittent
FROM osm_waterway_linestring
WHERE waterway IN ('river', 'canal', 'stream', 'drain', 'ditch')
    );

-- etldoc: osm_waterway_linestring ->  waterway_z14
CREATE OR REPLACE VIEW waterway_z14 AS
(
SELECT geometry,
       waterway::text AS class,
       name,
       name_en,
       name_de,
       tags,
       is_bridge,
       is_tunnel,
       is_intermittent
FROM osm_waterway_linestring
    );

-- etldoc: layer_waterway[shape=record fillcolor=lightpink, style="rounded,filled",
-- etldoc: label="layer_waterway | <z3> z3 |<z4> z4 |<z5> z5 |<z6> z6 |<z7> z7 |<z8> z8 | <z9> z9 |<z10> z10 |<z11> z11 |<z12> z12|<z13> z13|<z14> z14+" ];

CREATE OR REPLACE FUNCTION layer_waterway(bbox geometry, zoom_level int)
    RETURNS TABLE
            (
                geometry     geometry,
                class        text,
                name         text,
                name_en      text,
                name_de      text,
                brunnel      text,
                intermittent int,
                tags         hstore
            )
AS
$$
SELECT geometry,
       class,
       NULLIF(name, '') AS name,
       COALESCE(NULLIF(name_en, ''), NULLIF(name, '')) AS name_en,
       COALESCE(NULLIF(name_de, ''), NULLIF(name, ''), NULLIF(name_en, '')) AS name_de,
       waterway_brunnel(is_bridge, is_tunnel) AS brunnel,
       is_intermittent::int AS intermittent,
       tags
FROM (
         -- etldoc: waterway_z3 ->  layer_waterway:z3
         SELECT *
         FROM waterway_z3
         WHERE zoom_level = 3
         UNION ALL
         -- etldoc: waterway_z4 ->  layer_waterway:z4
         SELECT *
         FROM waterway_z4
         WHERE zoom_level = 4
         UNION ALL
         -- etldoc: waterway_z5 ->  layer_waterway:z5
         SELECT *
         FROM waterway_z5
         WHERE zoom_level = 5
         UNION ALL
         -- etldoc: waterway_z6 ->  layer_waterway:z6
         SELECT *
         FROM waterway_z6
         WHERE zoom_level = 6
         UNION ALL
         -- etldoc: waterway_z7 ->  layer_waterway:z7
         SELECT *
         FROM waterway_z7
         WHERE zoom_level = 7
         UNION ALL
         -- etldoc: waterway_z8 ->  layer_waterway:z8
         SELECT *
         FROM waterway_z8
         WHERE zoom_level = 8
         UNION ALL
         -- etldoc: waterway_z9 ->  layer_waterway:z9
         SELECT *
         FROM waterway_z9
         WHERE zoom_level = 9
         UNION ALL
         -- etldoc: waterway_z10 ->  layer_waterway:z10
         SELECT *
         FROM waterway_z10
         WHERE zoom_level = 10
         UNION ALL
         -- etldoc: waterway_z11 ->  layer_waterway:z11
         SELECT *
         FROM waterway_z11
         WHERE zoom_level = 11
         UNION ALL
         -- etldoc: waterway_z12 ->  layer_waterway:z12
         SELECT *
         FROM waterway_z12
         WHERE zoom_level = 12
         UNION ALL
         -- etldoc: waterway_z13 ->  layer_waterway:z13
         SELECT *
         FROM waterway_z13
         WHERE zoom_level = 13
         UNION ALL
         -- etldoc: waterway_z14 ->  layer_waterway:z14
         SELECT *
         FROM waterway_z14
         WHERE zoom_level >= 14
     ) AS zoom_levels
WHERE geometry && bbox;
$$ LANGUAGE SQL STABLE
                -- STRICT
                PARALLEL SAFE;

DO $$ BEGIN RAISE NOTICE 'Finished layer waterway'; END$$;