# website

## Build and run instructions

Install Vagrant and VirtualBox.
After running `vagrant up`, run `vagrant rsync-auto` in the background so that file changes on your host machine can be propagated to the virtual machine.
Use the following git aliases to make it easier to run the custom provision commands:

    [alias]
        build = !vagrant provision --provision-with build
        deploy = !vagrant provision --provision-with deploy && sftp -P 2822 -b getTarBz.sh root@localhost && sftp -b putTarBz.sh root@example.com
        run = !vagrant provision --provision-with run
        runconf = !vagrant provision --provision-with runconf
        test = !vagrant provision --provision-with test

## Serving files

Create a directory called `static`, and place some markdown files with the `.md` file extension.
One of these files must be called `index.md` as it serves as the main page of the website.

To host images, create a directory in `static` called `img` (i.e. `static/img`).
Place some image files in `img`, and refer to these images in the markdown files using `image/file.png`.

## Running on a web server

tar -xvjf website.tar.bz
./website config.json

bg %1
disown -h %1
