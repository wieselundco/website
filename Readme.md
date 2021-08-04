

## Notes for publishers


[![Netlify Status](https://api.netlify.com/api/v1/badges/daf44d97-4a2f-4da6-88b2-d45ea8bc36b1/deploy-status)](https://app.netlify.com/sites/wieselundco/deploys)

I am publishing this website via netlify, because [website redirects](https://docs.netlify.com/routing/redirects/) are very simple with netlify. My (Nils) netlify account has been granted access to all [wieselundco github repos](https://github.com/wieselundco) and I created an netlify site [wieselundco](https://app.netlify.com/sites/wieselundco). Everytime I push to https://github.com/wieselundco/website, netlify pulls the *Publish directory*, builds the site and publishes it to [wieselundco.netlify.app](https://wieselundco.netlify.app). 

In the very near future, I need to create a CNAME record in metanet which points wieselundco.ch to https://wieselundco.netlify.app/. Then, I need to [Enable automatic TLS certificates with Let’s Encrypt](https://app.netlify.com/sites/wieselundco/settings/domain#https).

