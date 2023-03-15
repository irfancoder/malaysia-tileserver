<!-- @format -->

# tileserver

Contains an open-source map server, [tileserver-gl](http://tileserver.org/) for serving vector & raster tiles & the Malaysia-Singapore-Brunei map. The map is generated from [openmaptiles](https://github.com/openmaptiles/openmaptiles).

---

## Prerequisites

0. Install the following:

-   Docker
-   Docker Compose

1. First, copy the `docker-compose.example` file into `docker-compose.yml` and add a `/data` folder (if missing)

```bash
cp docker-compose.example docker-compose.yml
mkdir data
```

2. Download the map data: [Malaysia-Singapore-Brunei](https://drive.google.com/file/d/1f9MllVDNz9BSBuIFCkST55tq14F2CDMP/view?usp=share_link) and extract it inside `/data`

---

## Usage

To run the server:

```bash
docker-compose up
```

To stop the server:

```bash
docker-compose down
```

## Todo

-   Add more map styles (dark mode, satellite etc)
