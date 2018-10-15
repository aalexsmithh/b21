# b21-backend

## Timesheet generator

### Dependencies

`timesheet/make-timesheet.sh` requires the following external programs at
runtime:
* `bc` is used to compute the total hours and total pay
* `imagemagick` is used to create the PDF based on `timesheet/casual_timesheet.png`.

To build the website the following tools are needed:
* (`stack`)[https://haskellstack.org]
* (`npm`)[https://npmjs.com]

### Installation

#### Basic setup and compilation

It is recommended to use a separate Unix user to run b21-backend, as a security
measure. Suppose the username is `b21`.

```
ssh b21@your-host
git clone https://github.com/building21/website.git
cd website
npm install
./build-frontend.sh
stack install # prepare to wait a long time
```

That last step should install the necessary Haskell compiler, download and build
all Haskell dependencies, and copy the built executables into
`$HOME/.local/bin`, in particular the `b21-backend-exe` program.

#### Deploy the frontend

Create the directory `/srv/http/b21` and `chown` it to the `b21` user.
Run `./deploy.sh`. This will copy the compiled frontend into `/srv/http/b21`.

#### Create the systemd unit file for the backend

It is not necessary to use systemd, but it's what I use so that's why I have
instructions for it.

Copy `systemd/b21-backend.service` to `/etc/systemd/system`.
Adjust paths involving usernames to use `b21`.

Create the directory `~/pdf`.

Create the file `~/b21-backend.production.env` with the following contents.

```
B21_API_PORT=8082
B21_TIMESHEET_SCRIPT=/home/b21/website/backend/timesheet/make-timesheet.sh
B21_PDF_DIR=pdf
B21_STATIC_DIR=/srv/http/b21/static
B21_STATIC_URL_PREFIX=http://your-host/static
B21_TIMESHEET_BASE=/home/b21/opt/casual_timesheet.png
```

Adjust `your-host` as necessary.

#### Start the backend

Run `systemctl start b21-backend` and optionally `systemctl enable b21-backend`.

#### Configure nginx

We need nginx to serve static files from `/srv/http/b21` and to proxy requests
under the `/api` path to the backend. Use a `server` block like the following.

```
server {
    listen 80;
    
    location / {
        root /srv/http/b21;
        index index.html;
    }
    
    location ~/api(.*)$ {
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $remote_addr;
        proxy_set_header Host $host;
        proxy_pass http://127.0.0.1:8082$1;
    }
    
    error_page 500 502 503 504 /50x.html;
    location = /50x.html {
        root /usr/share/nginx/html;
    }
}
```

Everything should work now!

### Updating

To update the site to a new version, it should suffice to perform the following
steps.

```
stack install
./build-frontend.sh
./deploy.sh
sudo systemctl restart b21-backend
```

Updates can be performed via the script `./remote-deploy.sh`, which will ssh
into the host, pull the lastest version from git, and perform all compilations,
deployment, and restarting. Using this script will require that the `b21` user
be permitted to execute `systemctl restart b21-backend` and `systemctl status
b21-backend` via `sudo` with no password.

To configure `remote-deploy.sh`, modify the file `.remote-deploy.env` with the
appropriate user, port, and hostname.

