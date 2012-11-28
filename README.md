# Licentious

Licentious is a Yesod app which allows a user to sign in with GitHub
then choose a repository and a license type. The license is committed
to the repository as `LICENSE.txt`.

## Running

A GitHub API Application is required. The application's key and secret
must be set in `Foundation.hs`:

```
authPlugins _ = [authGitHub "key" "secret"]
```

After that, you should be able to run `yesod devel`.

## Adding a license

Add a .txt file to `static/` and add the file and name of the license
to `licensesNames` in `Handler/Home.hs`. Done!

## Screenshots

![](http://licentious.herokuapp.com/static/img/step1.png)

![](http://licentious.herokuapp.com/static/img/step2.png)

![](http://licentious.herokuapp.com/static/img/step3.png)

![](http://licentious.herokuapp.com/static/img/step4.png)
