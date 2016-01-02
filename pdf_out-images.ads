private package PDF_Out.Images is

  --  Gives the image index for a given file name.
  --  A new index will be created if the image is not yet registered.
  --
  procedure Image_ref(pdf: in out PDF_Out_Stream; file_name: String; image_index: out Positive);

  procedure Clear_local_resource_flags( pdf: PDF_Out_Stream );

  --  Insert an image into the PDF document.
  --
  procedure Insert_Image_as_XObject(pdf: in out PDF_Out_Stream; image_index: Positive);

end PDF_Out.Images;
