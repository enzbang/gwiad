
private with AWS.Services.Dispatchers.URI;
private with AWS.Status;
private with AWS.Response;

package Services_Admin is

private
   use AWS;

   Main_Dispatcher : Services.Dispatchers.URI.Handler;

   function Default_Callback (Request : in Status.Data) return Response.Data;
   --  Registers default callback

end Services_Admin;
