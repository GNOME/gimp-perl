/*
 * extra functions used by gimp-perl, duplicated in each translation unit :/
 */

#ifndef EXTRA_C
#define EXTRA_C

#include <glib-object.h>

#include <gtk2perl-autogen.h>

/* need to factor these out, otherwise we always need gtk :( */
#include <libgimp/gimp.h>
#include <libgimpwidgets/gimpchainbutton.h>
#include <libgimpbase/gimpbasetypes.h>

#include "extra.h"

#include "gimpenums.c"

typedef struct {
  int e;
  gchar *s;
} enum_entry;

static void
sv_color3 (SV *sv, gdouble *e, gdouble *f, gdouble *g, gdouble *a)
{
  if (!SvROK (sv)
      || SvTYPE (SvRV (sv)) != SVt_PVAV
      || av_len ((AV *)SvRV (sv)) < 2
      || av_len ((AV *)SvRV (sv)) > 3)
    croak ("GimpRGB/HSV/HLS must be specified as an arrayref with length three or four");

  *e = SvNV (*av_fetch ((AV *)SvRV (sv), 0, 1));
  *f = SvNV (*av_fetch ((AV *)SvRV (sv), 1, 1));
  *g = SvNV (*av_fetch ((AV *)SvRV (sv), 2, 1));
  *a = av_len ((AV *)SvRV (sv)) < 3
             ? 1.
             : SvNV (*av_fetch ((AV *)SvRV (sv), 3, 1));
}

static SV *
newSV_color3 (gdouble e, gdouble f, gdouble g, gdouble a)
{
  AV *av = newAV ();

  av_push (av, newSVnv (e));
  av_push (av, newSVnv (f));
  av_push (av, newSVnv (g));
  av_push (av, newSVnv (a));

  return newRV_noinc ((SV *)av);
}

#define ENUM(name)			\
  static GType t_ ## name;		\
  static GType name ## _type (void)	\
  {					\
    if (!t_ ## name)			\
      t_ ## name = g_enum_register_static (# name, _ ## name ## _values);	\
    return t_ ## name;			\
  }

ENUM(gimp_unit)
ENUM(gimp_color_selector_channel)
ENUM(gimp_color_area_type)
ENUM(gimp_chain_position)
ENUM(gimp_size_entry_update_policy)

#define SvGimpRGB(sv, color) sv_color3 ((sv), &(color).r, &(color).g, &(color).b, &(color).a)
#define SvGimpHSV(sv, color) sv_color3 ((sv), &(color).h, &(color).s, &(color).v, &(color).a)

#define newSVGimpRGB(color) newSV_color3 ((color).r, (color).g, (color).b, (color).a)
#define newSVGimpHSV(color) newSV_color3 ((color).h, (color).s, (color).v, (color).a)

#endif

